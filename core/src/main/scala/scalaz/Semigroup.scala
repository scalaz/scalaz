package scalaz

trait Semigroup[F]  { self =>
  ////

  def append(f1: F, f2: => F): F

  // derived functions

  ////
  val semigroupSyntax = new scalaz.syntax.SemigroupSyntax[F] {}
}

////
/**
 *
 */
////

object Semigroup {
  def apply[F](implicit F: Semigroup[F]): Semigroup[F] = F

  ////

  ////
}

