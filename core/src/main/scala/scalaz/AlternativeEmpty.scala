package scalaz

////
/**
 *
 */
////
trait AlternativeEmpty[F[_]] extends Alternative[F] { self =>
  ////
  def empty[A]: F[A]
  // derived functions

  ////
  val alternativeEmptySyntax = new scalaz.syntax.AlternativeEmptySyntax[F] {}
}

object AlternativeEmpty {
  @inline def apply[F[_]](implicit F: AlternativeEmpty[F]): AlternativeEmpty[F] = F

  ////

  ////
}

