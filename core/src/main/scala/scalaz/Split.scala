package scalaz

////
/**
 * A [[scalaz.Compose]] (semigroupoid) permitting products.
 */
////
trait Split[=>:[_, _]] extends Compose[=>:] { self =>
  ////
  def split[A, B, C, D](f: A =>: B, g: C =>: D): (A,  C) =>: (B, D)

  // derived functions

  ////
  @transient lazy val splitSyntax = new scalaz.syntax.SplitSyntax[=>:] { def F = Split.this }
}

object Split {
  @inline def apply[F[_, _]](implicit F: Split[F]): Split[F] = F

  ////
  ////
}
