package scalaz

trait SemigroupLike[F]  { self =>
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
trait Semigroup[F] extends SemigroupLike[F] {
  self  =>


}

object Semigroup {
  def apply[F](implicit F: Semigroup[F]): Semigroup[F] = F

  ////

  ////
}

trait SemigroupInstance[F] extends Semigroup[F]
