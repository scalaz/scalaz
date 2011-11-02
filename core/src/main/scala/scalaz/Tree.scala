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
    Monoid[B].append(f(rootLabel), Traverse[Stream].foldMap[Tree[A], B](subForest)((_: Tree[A]).foldMap(f)))

  /** A 2D String representation of this Tree. */
  def drawTree(implicit sh: Show[A]): String =
    Traverse[Stream].foldMap(draw)((_: String) + "\n")

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
      s.zip(first #:: Stream.continually(other)).map {
        case (a, b) => a + b
      }

    sh.shows(rootLabel) #:: drawSubTrees(subForest)
  }

  /** Pre-order traversal. */
  def flatten: Stream[A] = squish(Stream.Empty)

  private def squish(xs: Stream[A]): Stream[A] =
    Stream.cons(rootLabel, Traverse[Stream].foldR[Tree[A], Stream[A]](subForest, xs)(a => b => a.squish(b)))

  /** Breadth-first traversal. */
  def levels: Stream[Stream[A]] = {
    val f = (s: Stream[Tree[A]]) => {
      Traverse[Stream].foldMap(s)((_: Tree[A]).subForest)
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
}

object Tree extends TreeFunctions with TreeInstances {
  /** Construct a tree node with no children. */
  def apply[A](root: => A): Tree[A] = leaf(root)

  object Node {
    def unapply[A](t: Tree[A]): Option[(A, Stream[Tree[A]])] = Some((t.rootLabel, t.subForest))
  }
}

trait TreeInstances {
  // TODO
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
