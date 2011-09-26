package scalaz

trait MonoidLike[F] extends Semigroup[F] { self =>
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
trait Monoid[F] extends MonoidLike[F] {
  self  =>


}

object Monoid {
  def apply[F](implicit F: Monoid[F]): Monoid[F] = F

  ////

  ////
}

trait MonoidInstance[F] extends Monoid[F] with SemigroupInstance[F]
