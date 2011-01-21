package scalaz

/**
 * A multi-way tree, also known as a rose tree. Also known as Cofree[Stream, A].
 */
sealed trait Tree[+A] {

  /** The label at the root of this tree. */
  def rootLabel: A

  /** The child nodes of this tree. */
  def subForest: Stream[Tree[A]]

  import Scalaz._

  /** Maps the elements of the Tree into a Monoid and folds the resulting Tree. */
  def foldMap[B: Monoid](f: A => B): B =
    f(rootLabel) ⊹ subForest.foldMap((_: Tree[A]).foldMap(f))

  /** A 2D String representation of this Tree. */
  def drawTree[B >: A](implicit sh: Show[B]): String = {
    implicit val showa: Show[A] = sh comap (x => x)
    draw.foldMap(_ + "\n")
  }

  /** A histomorphic transform. Each element in the resulting tree
    * is a function of the corresponding element in this tree
    * and the histomorphic transform of its children. */
  def scanr[B](g: (A, Stream[Tree[B]]) => B): Tree[B] = {
    lazy val c = subForest.map(_.scanr(g))
    node(g(rootLabel, c), c)
  }

  /** A 2D String representation of this Tree, separated into lines. */
  def draw[B >: A](implicit sh: Show[B]): Stream[String] = {
    implicit val showa: Show[A] = sh comap (x => x)
    def drawSubTrees(s: Stream[Tree[A]]): Stream[String] = s match {
      case Stream.Empty => Stream.Empty
      case Stream(t) => "|" #:: shift("`- ", "   ", t.draw)
      case t #:: ts => "|" #:: shift("+- ", "|  ", t.draw) append drawSubTrees(ts)
    }
    def shift(first: String, other: String, s: Stream[String]): Stream[String] =
      s.ʐ <*> ((first #:: other.repeat[Stream]).ʐ ∘ ((_: String) + (_: String)).curried)

    rootLabel.shows #:: drawSubTrees(subForest)
  }

  /** Pre-order traversal. */
  def flatten: Stream[A] = squish[A](Stream.Empty)

  private def squish[AA >: A](xs: Stream[AA]): Stream[AA] =
    Stream.cons(rootLabel, subForest.foldr(xs)(_.squish(_)))

  /** Breadth-first traversal. */
  def levels: Stream[Stream[A]] = {
    val f = (s: Stream[Tree[A]]) => s.foldMap(_.subForest)
    Stream(this).iterate[Stream](f).takeWhile(!_.isEmpty) ∘∘ ((_: Tree[A]).rootLabel)
  }

  /** Binds the given function across all the subtrees of this tree. */
  def cobind[B](f: Tree[A] => B): Tree[B] = this.unfoldTree((t: Tree[A]) => (f(t), () => t.subForest))

  /** A TreeLoc zipper of this tree, focused on the root node. */
  def loc: TreeLoc[A] = Scalaz.loc(this, Stream.Empty, Stream.Empty, Stream.Empty)

  /** Turns a tree of pairs into a pair of trees. */
  def unzip[A1, A2](implicit p: A => (A1, A2)): (Tree[A1], Tree[A2]) = {
    lazy val uz = subForest.map(_.unzip)
    lazy val fst = uz map (_._1)
    lazy val snd = uz map (_._2)
    (node(rootLabel._1, fst), node(rootLabel._2, snd))
  }
}
  
trait Trees {
  /** Construct a new Tree node. */
  def node[A](root: => A, forest: => Stream[Tree[A]]): Tree[A] = new Tree[A] {
    lazy val rootLabel = root
    lazy val subForest = forest
    override def toString = "<tree>"
  }

  /** Construct a tree node with no children. */
  def leaf[A](root: => A): Tree[A] = node(root, Stream.empty)
}
