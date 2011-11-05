package scalaz

trait CoPointed[F[_]] extends Functor[F] { self =>
  ////
  def copure[A](p: F[A]): A

  // derived functions

  ////
  val coPointedSyntax = new scalaz.syntax.CoPointedSyntax[F] {}
}

object CoPointed {
  def apply[F[_]](implicit F: CoPointed[F]): CoPointed[F] = F

  ////

  ////
}

