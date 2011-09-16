package scalaz

trait MonoidLike[F]  { self =>
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
trait Monoid[F] extends MonoidLike[F]

trait MonoidInstance[F] extends Monoid[F]
