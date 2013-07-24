package scalaz

import std.stream.{streamInstance, streamMonoid}
import std.string.stringInstance

/**
 * A multi-way tree, also known as a rose tree. Also known as Cofree[Stream, A].
 */
sealed trait Tree[A] {

  import Tree._

  /** The label at the root of this tree. */
  def rootLabel: A

  /** The child nodes of this tree. */
  def subForest: Stream[Tree[A]]

  /** Maps the elements of the Tree into a Monoid and folds the resulting Tree. */
  def foldMap[B: Monoid](f: A => B): B =
    Monoid[B].append(f(rootLabel), Foldable[Stream].foldMap[Tree[A], B](subForest)((_: Tree[A]).foldMap(f)))

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    Foldable[Stream].foldRight(flatten, z)(f)

  /** A 2D String representation of this Tree. */
  def drawTree(implicit sh: Show[A]): String =
    Foldable[Stream].foldMap(draw)((_: String) + "\n")

  /** A histomorphic transform. Each element in the resulting tree
   * is a function of the corresponding element in this tree
   * and the histomorphic transform of its children.
   **/
  def scanr[B](g: (A, Stream[Tree[B]]) => B): Tree[B] = {
    lazy val c = subForest.map(_.scanr(g))
    node(g(rootLabel, c), c)
  }

  /** A 2D String representation of this Tree, separated into lines. */
  def draw(implicit sh: Show[A]): Stream[String] = {
    def drawSubTrees(s: Stream[Tree[A]]): Stream[String] = s match {
      case Stream.Empty => Stream.Empty
      case Stream(t)    => "|" #:: shift("`- ", "   ", t.draw)
      case t #:: ts     => "|" #:: shift("+- ", "|  ", t.draw) append drawSubTrees(ts)
    }
    def shift(first: String, other: String, s: Stream[String]): Stream[String] =
      (first #:: Stream.continually(other)).zip(s).map {
        case (a, b) => a + b
      }

    sh.shows(rootLabel) #:: drawSubTrees(subForest)
  }

  /** Pre-order traversal. */
  def flatten: Stream[A] = {
    def squish(tree: Tree[A], xs: Stream[A]): Stream[A] =
      Stream.cons(tree.rootLabel, Foldable[Stream].foldr[Tree[A], Stream[A]](tree.subForest, xs)(a => b => squish(a, b)))

    squish(this, Stream.Empty)
  }

  /** Breadth-first traversal. */
  def levels: Stream[Stream[A]] = {
    val f = (s: Stream[Tree[A]]) => {
      Foldable[Stream].foldMap(s)((_: Tree[A]).subForest)
    }
    Stream.iterate(Stream(this))(f) takeWhile (!_.isEmpty) map (_ map (_.rootLabel))
  }

  /** Binds the given function across all the subtrees of this tree. */
  def cobind[B](f: Tree[A] => B): Tree[B] = unfoldTree(this)(t => (f(t), () => t.subForest))

  /** A TreeLoc zipper of this tree, focused on the root node. */
  def loc: TreeLoc[A] = TreeLoc.loc(this, Stream.Empty, Stream.Empty, Stream.Empty)

  /** Turns a tree of pairs into a pair of trees. */
  def unzip[A1, A2](implicit p: A => (A1, A2)): (Tree[A1], Tree[A2]) = {
    lazy val uz = subForest.map(_.unzip)
    lazy val fst = uz map (_._1)
    lazy val snd = uz map (_._2)
    (node(rootLabel._1, fst), node(rootLabel._2, snd))
  }

  def foldNode[Z](f: A => Stream[Tree[A]] => Z): Z =
    f(rootLabel)(subForest)

  def map[B](f: A => B): Tree[B] =
    node(f(rootLabel), subForest map (_ map f))

  def flatMap[B](f: A => Tree[B]): Tree[B] = {
    val r: Tree[B] = f(rootLabel)
    Tree.node(r.rootLabel, r.subForest #::: subForest.map(_.flatMap(f)))
  }

  def traverse1[G[_] : Apply, B](f: A => G[B]): G[Tree[B]] = {
    val G = Apply[G]
    import Stream._
    subForest match {
      case Empty => G.map(f(rootLabel))(Tree(_))
      case x #:: xs => G.apply2(f(rootLabel), NonEmptyList.nel(x, xs.toList).traverse1(_.traverse1(f))) {
        case (h, t) => Tree.node(h, t.list.toStream)
      }
    }
  }
}

object Tree extends TreeFunctions with TreeInstances {
  /** Construct a tree node with no children. */
  def apply[A](root: => A): Tree[A] = leaf(root)

  object Node {
    def unapply[A](t: Tree[A]): Option[(A, Stream[Tree[A]])] = Some((t.rootLabel, t.subForest))
  }


}

trait TreeInstances {
  implicit val treeInstance: Traverse1[Tree] with Monad[Tree] with Comonad[Tree] = new Traverse1[Tree] with Monad[Tree] with Comonad[Tree] {
    def point[A](a: => A): Tree[A] = Tree.leaf(a)
    def cobind[A, B](fa: Tree[A])(f: Tree[A] => B): Tree[B] = fa cobind f
    def copoint[A](p: Tree[A]): A = p.rootLabel
    override def map[A, B](fa: Tree[A])(f: A => B) = fa map f
    def bind[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa flatMap f
    def traverse1Impl[G[_]: Apply, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] = fa traverse1 f
    override def foldRight[A, B](fa: Tree[A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
    override def foldRight1[A](fa: Tree[A])(f: (A, => A) => A): A = fa.subForest.foldRight(fa.rootLabel)((t, a) => treeInstance.foldRight(t, a)(f))
    override def foldLeft[A, B](fa: Tree[A], z: B)(f: (B, A) => B): B =
      fa.flatten.foldLeft(z)(f)
    override def foldLeft1[A](fa: Tree[A])(f: (A, A) => A): A = fa.flatten match {
      case h #:: t => t.foldLeft(h)(f)
    }
    override def foldMap[A, B](fa: Tree[A])(f: A => B)(implicit F: Monoid[B]): B = fa foldMap f
  }

  implicit def treeEqual[A](implicit A: Equal[A]): Equal[Tree[A]] = new Equal[Tree[A]] {
    def equal(a1: Tree[A], a2: Tree[A]): Boolean = {
      A.equal(a1.rootLabel, a2.rootLabel) && a1.subForest.corresponds(a2.subForest)(equal _)
    }
  }

  /* TODO
  def applic[A, B](f: Tree[A => B]) = a => Tree.node((f.rootLabel)(a.rootLabel), implicitly[Applic[newtypes.ZipStream]].applic(f.subForest.map(applic[A, B](_)).ʐ)(a.subForest ʐ).value)
   */
}

trait TreeFunctions {
  /** Construct a new Tree node. */
  def node[A](root: => A, forest: => Stream[Tree[A]]): Tree[A] = new Tree[A] {
    lazy val rootLabel = root
    lazy val subForest = forest

    override def toString = "<tree>"
  }

  /** Construct a tree node with no children. */
  def leaf[A](root: => A): Tree[A] = node(root, Stream.empty)

  def unfoldForest[A, B](s: Stream[A])(f: A => (B, () => Stream[A])): Stream[Tree[B]] =
    s.map(unfoldTree(_)(f))

  def unfoldTree[A, B](v: A)(f: A => (B, () => Stream[A])): Tree[B] =
    f(v) match {
      case (a, bs) => node(a, unfoldForest(bs.apply())(f))
    }
}
