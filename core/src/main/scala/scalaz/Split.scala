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
  val splitSyntax = new scalaz.syntax.SplitSyntax[=>:] { def F = Split.this }
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

trait IsomorphismSplit[F[_, _], G[_, _]] extends Split[F] with IsomorphismCompose[F, G]{
  implicit def G: Split[G]
  ////

  override def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A,  C), (B, D)] =
    iso.from(G.split(iso.to(f), iso.to(g)))
  ////
}
