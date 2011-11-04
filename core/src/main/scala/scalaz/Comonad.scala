package scalaz

trait Comonad[F[_]] extends Copointed[F] with Cojoin[F] with Cobind[F] { self =>

  ////

  // derived functions

  ////

  val comonadSyntax = new scalaz.syntax.ComonadSyntax[F] {}
}

object Comonad {
  def apply[F[_]](implicit F: Comonad[F]): Comonad[F] = F

  ////

  ////
}

