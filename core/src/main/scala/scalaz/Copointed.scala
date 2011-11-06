package scalaz

trait CoPointed[F[_]] extends Functor[F] { self =>
  ////
  def copoint[A](p: F[A]): A

  // derived functions

  /** alias for `copoint` */
  def copure[A](p: F[A]): A = copoint(p)

  ////
  val coPointedSyntax = new scalaz.syntax.CoPointedSyntax[F] {}
}

object CoPointed {
  @inline def apply[F[_]](implicit F: CoPointed[F]): CoPointed[F] = F

  ////

  ////
}

