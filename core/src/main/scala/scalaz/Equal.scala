package scalaz

trait Equal[F]  { self =>
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

object Equal {
  def apply[F](implicit F: Equal[F]): Equal[F] = F

  ////

  ////
}

