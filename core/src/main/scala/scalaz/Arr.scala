package scalaz

trait Arr[F[_, _]] {
  ////
  def arr[A, B](f: A => B): F[A, B]

  // derived functions

  ////

  val emptySyntax = new scalaz.syntax.ArrSyntax[F] {}
}


object Arr {
  def apply[F[_, _]](implicit F: Arr[F]): Arr[F] = F

  ////
  // TODO CokleisliArr instance
  ////
}
