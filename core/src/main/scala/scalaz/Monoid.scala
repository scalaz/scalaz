package scalaz

trait Monoid[F] extends Semigroup[F] { self =>
  ////
  def zero: F

  // derived functions

  ////
  val monoidSyntax = new scalaz.syntax.MonoidSyntax[F] {}
}

////
/**
 *
 */
////

object Monoid {
  def apply[F](implicit F: Monoid[F]): Monoid[F] = F

  ////

  ////
}

