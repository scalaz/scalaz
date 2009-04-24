package scalaz

/**
 * A multi-way tree, also known as a rose tree.
 */
sealed trait Tree[+A] {
  val rootLabel: A
  val subForest: Stream[Tree[A]]

  import S._
  import MA._
  import ZipStream._
  import Functor._

  def foldMap[B](f: A => B)(implicit m: Monoid[B]):B =
    f(rootLabel) |+| subForest.foldMap((_:Tree[A]).foldMap(f))

  def drawTree(implicit sh: Show[A]) = draw.foldMap(_ + "\n")

  def draw(implicit sh: Show[A]): Stream[String] = {
    def drawSubTrees(s: Stream[Tree[A]]):Stream[String] = s match {
      case Stream.empty => Stream.empty
      case Stream.cons(t, Stream.empty) => Stream.cons("|", shift("`- ", "   ", t.draw))
      case Stream.cons(t, ts) => Stream.cons("|", shift("+- ", "|  ", t.draw)) append drawSubTrees(ts)
    }
    def shift(first: String, other: String, s: Stream[String]) =
      Stream.cons(first, other.repeat[Stream]).zipWith(((_:String) + (_:String)).curry, S.zip(s))
    Stream.cons(rootLabel.shows, drawSubTrees(subForest))
  }
}

object Tree {
  def node[A](root: A, forest: Stream[Tree[A]]) = new Tree[A] {
    val rootLabel = root
    val subForest = forest
  }
}