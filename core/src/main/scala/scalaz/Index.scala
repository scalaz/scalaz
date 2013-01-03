package scalaz

////
/**
 * Random access through an integer index.
 */
////
trait Index[F[_]]  { self =>
  ////
  /**
   * @return the element at index `i` in a `Some`, or `None` if the given index falls outside of the range
   */
  def index[A](fa: F[A], i: Int): Option[A]

  /**
   * @return the element at index `i`, or `default` if the given index falls outside of the range
   */
  def indexOr[A](fa: F[A], default: => A, i: Int): A =
    index(fa, i) getOrElse default

  // derived functions

  ////
  val indexSyntax = new scalaz.syntax.IndexSyntax[F] { def F = Index.this }
}

object Index {
  @inline def apply[F[_]](implicit F: Index[F]): Index[F] = F

  ////

  ////
}
