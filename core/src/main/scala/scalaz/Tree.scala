package scalaz

import std.stream.{streamInstance, streamMonoid}
import std.string.stringInstance
import Free.Trampoline

/**
 * A multi-way tree, also known as a rose tree. Also known as Cofree[Stream, A].
 */
sealed abstract class Tree[A] {

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
  def drawTree(implicit sh: Show[A]): String = {
    val reversedLines = draw.run
    val first = new StringBuilder(reversedLines.head.toString.reverse)
    val rest = reversedLines.tail
    rest.foldLeft(first) { (acc, elem) => 
      acc.append("\n").append(elem.toString.reverse) 
    }.append("\n").toString
  }

  /** A histomorphic transform. Each element in the resulting tree
   * is a function of the corresponding element in this tree
   * and the histomorphic transform of its children.
   **/
  def scanr[B](g: (A, Stream[Tree[B]]) => B): Tree[B] = {
    lazy val c = subForest.map(_.scanr(g))
    node(g(rootLabel, c), c)
  }

  /** A 2D String representation of this Tree, separated into lines. 
    * Uses reversed StringBuilders for performance, because they are
    * prepended to.
    **/
  private def draw(implicit sh: Show[A]): Trampoline[Vector[StringBuilder]] = {
    import Trampoline._
    val branch = " -+" // "+- ".reverse
    val stem = " -`" // "`- ".reverse
    val trunk = "  |" // "|  ".reverse

    def drawSubTrees(s: Stream[Tree[A]]): Trampoline[Vector[StringBuilder]] = s match {
      case ts if ts.isEmpty       => done(Vector.empty[StringBuilder])
      case t #:: ts if ts.isEmpty => suspend(t.draw).map(subtree => new StringBuilder("|") +: shift(stem, "   ", subtree))
      case t #:: ts               => for {
                                       subtree <- suspend(t.draw)
                                       otherSubtrees <- suspend(drawSubTrees(ts))
                                     } yield new StringBuilder("|") +: (shift(branch, trunk, subtree) ++ otherSubtrees)
    }

    def shift(first: String, other: String, s: Vector[StringBuilder]): Vector[StringBuilder] = {
      var i = 0
      while (i < s.length) {
        if (i == 0) s(i).append(first)
        else s(i).append(other)
        i += 1
      }
      s
    }
    
    drawSubTrees(subForest).map { subtrees => 
      new StringBuilder(sh.shows(rootLabel).reverse) +: subtrees
    }
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
      case x #:: xs => G.apply2(f(rootLabel), NonEmptyList.nel(x, IList.fromFoldable(xs)).traverse1(_.traverse1(f))) {
        case (h, t) => Tree.node(h, t.list.toStream)
      }
    }
  }
}

object Tree extends TreeInstances with TreeFunctions {
  /** Construct a tree node with no children. */
  def apply[A](root: => A): Tree[A] = leaf(root)

  object Node {
    def unapply[A](t: Tree[A]): Option[(A, Stream[Tree[A]])] = Some((t.rootLabel, t.subForest))
  }


}

sealed abstract class TreeInstances {
  implicit val treeInstance: Traverse1[Tree] with Monad[Tree] with Comonad[Tree] with Align[Tree] = new Traverse1[Tree] with Monad[Tree] with Comonad[Tree] with Align[Tree] {
    def point[A](a: => A): Tree[A] = Tree.leaf(a)
    def cobind[A, B](fa: Tree[A])(f: Tree[A] => B): Tree[B] = fa cobind f
    def copoint[A](p: Tree[A]): A = p.rootLabel
    override def map[A, B](fa: Tree[A])(f: A => B) = fa map f
    def bind[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa flatMap f
    def traverse1Impl[G[_]: Apply, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] = fa traverse1 f
    override def foldRight[A, B](fa: Tree[A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
    override def foldMapRight1[A, B](fa: Tree[A])(z: A => B)(f: (A, => B) => B) = (fa.flatten.reverse: @unchecked) match {
      case h #:: t => t.foldLeft(z(h))((b, a) => f(a, b))
    }
    override def foldLeft[A, B](fa: Tree[A], z: B)(f: (B, A) => B): B =
      fa.flatten.foldLeft(z)(f)
    override def foldMapLeft1[A, B](fa: Tree[A])(z: A => B)(f: (B, A) => B): B = fa.flatten match {
      case h #:: t => t.foldLeft(z(h))(f)
    }
    override def foldMap[A, B](fa: Tree[A])(f: A => B)(implicit F: Monoid[B]): B = fa foldMap f
    def alignWith[A, B, C](f: (\&/[A, B]) ⇒ C) = { 
      def align(ta: Tree[A], tb: Tree[B]): Tree[C] =
        Tree.node(f(\&/(ta.rootLabel, tb.rootLabel)), Align[Stream].alignWith[Tree[A], Tree[B], Tree[C]]({
          case \&/.This(sta) ⇒ sta map {a ⇒ f(\&/.This(a))}
          case \&/.That(stb) ⇒ stb map {b ⇒ f(\&/.That(b))}
          case \&/(sta, stb) ⇒ align(sta, stb)
        })(ta.subForest, tb.subForest))
      align _
    }
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
