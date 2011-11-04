package scalaz

trait Copointed[F[_]] extends Functor[F] { self =>
  ////
  def copure[A](p: F[A]): A

  // derived functions

  ////
  val copointedSyntax = new scalaz.syntax.CopointedSyntax[F] {}
}

object Copointed {
  def apply[F[_]](implicit F: Copointed[F]): Copointed[F] = F

  ////

  ////
}

