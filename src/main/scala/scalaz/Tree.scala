package scalaz

/**
 * A multi-way tree, also known as a rose tree.
 */
sealed trait Tree[+A] {
  val rootLabel: A
  val subForest: Stream[Tree[A]]

  import S._
  import MA._

  def foldMap[B](f: A => B)(implicit m: Monoid[B]):B =
    f(rootLabel) |+| subForest.foldMap((_:Tree[A]).foldMap(f))
}

object Tree {
  def node[A](root: A, forest: Stream[Tree[A]]) = new Tree[A] {
    val rootLabel = root
    val subForest = forest
  }
}