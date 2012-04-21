package scalaz

////
/**
 *
 */
////
trait Split[=>:[_, _]]  { self =>
  ////
  def split[A, B, C, D](f: A =>: B, g: C =>: D): (A,  C) =>: (B, D)

  // derived functions

  ////
  val splitSyntax = new scalaz.syntax.SplitSyntax[=>:] {}
}

object Split {
  @inline def apply[F[_, _]](implicit F: Split[F]): Split[F] = F

  ////
  ////
}

