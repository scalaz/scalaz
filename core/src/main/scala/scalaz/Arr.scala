package scalaz

trait Arr[=>:[_, _]]  { self =>
  ////
  def arr[A, B](f: A => B): A =>: B

  // derived functions

  ////
  val arrSyntax = new scalaz.syntax.ArrSyntax[=>:] {}
}

object Arr {
  def apply[F[_, _]](implicit F: Arr[F]): Arr[F] = F

  ////
  // TODO CokleisliArr instance
  ////
}

