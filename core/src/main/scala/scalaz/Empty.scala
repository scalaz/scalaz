package scalaz

trait Empty[F[_]]  { self =>
  ////
  def empty[A]: F[A]

  // derived functions

  ////
  val emptySyntax = new scalaz.syntax.EmptySyntax[F] {}
}

object Empty {
  @inline def apply[F[_]](implicit F: Empty[F]): Empty[F] = F

  ////

  ////
}

