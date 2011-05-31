package scalaz
package data

import wrap.StreamW._

/**
 * A multi-way tree, also known as a rose tree. Also known as Cofree[Stream, A].
 */
sealed trait Tree[A] {

  import Tree._

  /**The label at the root of this tree. */
  def rootLabel: A

  /**The child nodes of this tree. */
  def subForest: Stream[Tree[A]]

  /**Maps the elements of the Tree into a Monoid and folds the resulting Tree. */
  def foldMap[B: Monoid](f: A => B): B = {
    implicitly[Monoid[B]].append(
    f(rootLabel)
    , {
      val k = implicitly[Foldr[Stream]].foldMap[Tree[A], B]((_: Tree[A]).foldMap(f))
      k(subForest)
    }
    )
  }

  /**A 2D String representation of this Tree. */
  def drawTree(implicit sh: Show[A]): String = {
    val k = implicitly[Foldr[Stream]].foldMap((_: String) + "\n")
    k(draw)
  }

  /**A histomorphic transform. Each element in the resulting tree
   * is a function of the corresponding element in this tree
   * and the histomorphic transform of its children. */
  def scanr[B](g: (A, Stream[Tree[B]]) => B): Tree[B] = {
    lazy val c = subForest.map(_.scanr(g))
    node(g(rootLabel, c), c)
  }

  /**A 2D String representation of this Tree, separated into lines. */
  def draw(implicit sh: Show[A]): Stream[String] = {
    def drawSubTrees(s: Stream[Tree[A]]): Stream[String] = s match {
      case Stream.Empty => Stream.Empty
      case Stream(t) => "|" #:: shift("`- ", "   ", t.draw)
      case t #:: ts => "|" #:: shift("+- ", "|  ", t.draw) append drawSubTrees(ts)
    }
    def shift(first: String, other: String, s: Stream[String]): Stream[String] =
      s.zip(first #:: Stream.continually(other)).map {
        case (a, b) => a + b
      }

    sh.shows(rootLabel) #:: drawSubTrees(subForest)
  }

  /**Pre-order traversal. */
  def flatten: Stream[A] = squish(Stream.Empty)

  private def squish(xs: Stream[A]): Stream[A] =
    Stream.cons(rootLabel, implicitly[Foldr[Stream]].foldr[Tree[A], Stream[A]](a => b => a.squish(b))(xs)(subForest))

  /**Breadth-first traversal. */
  def levels: Stream[Stream[A]] = {
    val f = (s: Stream[Tree[A]]) => {
      val k = implicitly[Foldr[Stream]].foldMap((_: Tree[A]).subForest)
      k(s)
    }
    Stream.iterate(Stream(this))(f) takeWhile (!_.isEmpty) map (_ map (_.rootLabel))
  }

  /**Binds the given function across all the subtrees of this tree. */
  def cobind[B](f: Tree[A] => B): Tree[B] = unfoldTree(this, (t: Tree[A]) => (f(t), () => t.subForest))

  /**A TreeLoc zipper of this tree, focused on the root node. */
  def loc: TreeLoc[A] = TreeLoc.loc(this, Stream.Empty, Stream.Empty, Stream.Empty)

  /**Turns a tree of pairs into a pair of trees. */
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
}

object Tree extends Trees {
  /**Construct a tree node with no children. */
  def apply[A](root: => A): Tree[A] =
    leaf(root)
}

trait Trees {

  object Node {
    def unapply[A](t: Tree[A]): Option[(A, Stream[Tree[A]])] = Some((t.rootLabel, t.subForest))
  }

  /**Construct a new Tree node. */
  def node[A](root: => A, forest: => Stream[Tree[A]]): Tree[A] = new Tree[A] {
    lazy val rootLabel = root
    lazy val subForest = forest

    override def toString = "<tree>"
  }

  import newtypes._
  import wrap.StreamW._

  /**Construct a tree node with no children. */
  def leaf[A](root: => A): Tree[A] = node(root, Stream.empty)

  def unfoldForest[A, B](s: Stream[A], f: A => (B, () => Stream[A])): Stream[Tree[B]] =
    s.map(unfoldTree(_, f))

  def unfoldTree[A, B](v: A, f: A => (B, () => Stream[A])): Tree[B] =
    f(v) match {
      case (a, bs) => node(a, unfoldForest(bs.apply, f))
    }

  implicit def TreeShow[A: Show]: Show[Tree[A]] =
    Show.show((t: Tree[A]) =>
      '{' :: implicitly[Show[A]].show(t.rootLabel) ++ " " ++ implicitly[Show[Stream[Tree[A]]]].show(t.subForest) ++ "}")

  implicit def TreeEqual[A: Equal]: Equal[Tree[A]] =
    Equal.equalC[Tree[A]]((a1, a2) =>
      implicitly[Equal[A]].equal(a1.rootLabel)(a2.rootLabel)
          && implicitly[Equal[Iterable[Tree[A]]]].equal(a1.subForest)(a2.subForest))

  implicit val TreePointed: Pointed[Tree] = new Pointed[Tree] {
    def point[A](a: => A) = leaf(a)
  }

  implicit val TreeFunctor: Functor[Tree] = new Functor[Tree] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

  implicit val TreePointedFunctor: PointedFunctor[Tree] =
    PointedFunctor.pointedFunctor[Tree]

  implicit val TreeApplic: Applic[Tree] = new Applic[Tree] {
    def applic[A, B](f: Tree[A => B]) =
      a =>
        node((f.rootLabel)(a.rootLabel), implicitly[Applic[ZipStream]].applic(f.subForest.map(applic[A, B](_)).ʐ)(a.subForest ʐ).value)
  }

  implicit val TreeApplicFunctor: ApplicFunctor[Tree] =
    ApplicFunctor.applicFunctor[Tree]

  implicit val TreeApplicative: Applicative[Tree] =
    Applicative.applicative[Tree]

  implicit def TreeBind: Bind[Tree] = new Bind[Tree] {
    def bind[A, B](f: A => Tree[B]) =
      t => {
        val r = f(t.rootLabel)
        node(r.rootLabel, r.subForest #::: t.subForest.map(bind(f): Tree[A] => Tree[B]) )
      }
  }

  implicit val TreeBindFunctor: BindFunctor[Tree] =
    BindFunctor.bindFunctor[Tree]

  implicit val TreeMonad: Monad[Tree] =
    Monad.monadBP[Tree]

  implicit val TreeTraverse: Traverse[Tree] = new Traverse[Tree] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      ta => {
        val a = implicitly[Applicative[F]]
        a.apply(a.fmap((x: B) => (xs: Stream[Tree[B]]) => node(x, xs))(f(ta.rootLabel)))(implicitly[Traverse[Stream]].traverse[F, Tree[A], Tree[B]](traverse[F, A, B](f).apply(_: Tree[A])).apply(ta.subForest))
      }
  }

}
