package scalaz

/**
 * A multi-way tree, also known as a rose tree.
 */
sealed trait Tree[+A] {
  val rootLabel: A

  def subForest: Stream[Tree[A]]

  import Scalaz._

  def foldMap[B: Monoid](f: A => B): B =
    f(rootLabel) ⊹ subForest.foldMap((_: Tree[A]).foldMap(f))

  def drawTree[B >: A](implicit sh: Show[B]): String = {
    implicit val showa: Show[A] = sh comap (x => x)
    draw.foldMap(_ + "\n")
  }

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

  def flatten: Stream[A] = squish[A](Stream.Empty)

  private def squish[AA >: A](xs: Stream[AA]): Stream[AA] =
    Stream.cons(rootLabel, subForest.foldr(xs)(_.squish(_)))

  def levels: Stream[Stream[A]] = {
    val f = (s: Stream[Tree[A]]) => s.foldMap(_.subForest)
    Stream(this).iterate[Stream](f).takeWhile(!_.isEmpty) ∘∘ ((_: Tree[A]).rootLabel)
  }

  def cobind[B](f: Tree[A] => B): Tree[B] = this.unfoldTree((t: Tree[A]) => (f(t), () => t.subForest))

  def loc: TreeLoc[A] = Scalaz.loc(this, Stream.Empty, Stream.Empty, Stream.Empty)
}

trait Trees {
  def node[A](root: A, forest: Stream[Tree[A]]): Tree[A] = new Tree[A] {
    val rootLabel = root

    def subForest = forest


    override def toString = "<tree>"
  }

  def leaf[A](root: A): Tree[A] = node(root, Stream.empty)
}
