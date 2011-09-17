package scalaz

trait EqualLike[F]  { self =>
  ////
  def equal(a1: F, a2: F): Boolean

  // derived functions

  ////
  val equalSyntax = new scalaz.syntax.EqualSyntax[F] {}
}

////
/**
 *
 */
////
trait Equal[F] extends EqualLike[F]

object Equal {
  def apply[F](implicit F: Equal[F]): Equal[F] = F

  ////

  ////
}

trait EqualInstance[F] extends Equal[F]
