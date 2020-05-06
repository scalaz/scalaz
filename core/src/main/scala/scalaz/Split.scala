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
  val splitSyntax: scalaz.syntax.SplitSyntax[=>:] =
    new scalaz.syntax.SplitSyntax[=>:] { def F = Split.this }
}

object Split {
  @inline def apply[F[_, _]](implicit F: Split[F]): Split[F] = F

  import Isomorphism._

  def fromIso[F[_, _], G[_, _]](D: F <~~> G)(implicit E: Split[G]): Split[F] =
    new IsomorphismSplit[F, G] {
      override def G: Split[G] = E
      override def iso: F <~~> G = D
    }

  ////
  ////
}
