package scalaz

trait Index[F[_]] {
  self =>
  ////
  def index[A](fa: F[A], n: Int): Option[A]

  def indexOr[A](fa: F[A], default: => A, n: Int): A =
    index(fa, n) getOrElse default

  // derived functions

  ////
  val indexSyntax = new scalaz.syntax.IndexSyntax[F] {}
}

object Index {
  def apply[F[_]](implicit F: Index[F]): Index[F] = F

  ////

  ////
}

