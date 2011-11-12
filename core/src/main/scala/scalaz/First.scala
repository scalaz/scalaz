package scalaz

////
/**
 *
 */
////
trait First[F[_, _]]  { self =>
  ////
  def first[A, B, C](f: F[A, B]): F[(A, C), (B, C)]

  ////
  val firstSyntax = new scalaz.syntax.FirstSyntax[F] {}
}

object First {
  @inline def apply[F[_, _]](implicit F: First[F]): First[F] = F

  ////

  ////
}

