package scalaz

trait Cojoin[F[_]]  { self =>
  ////
  def cojoin[A](a: F[A]): F[F[A]]

  // derived functions

  ////
  val cojoinSyntax = new scalaz.syntax.CojoinSyntax[F] {}
}

////
/**
 *
 */
////

object Cojoin {
  def apply[F[_]](implicit F: Cojoin[F]): Cojoin[F] = F

  ////

  ////
}

