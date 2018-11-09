package scalaz

////
/**
 * Strength on a product.
 */
////
trait Strong[=>:[_, _]] extends Profunctor[=>:] { self =>
  ////
  def first[A, B, C](fa: A =>: B): (A, C) =>: (B, C)

  def second[A, B, C](fa: A =>: B): (C, A) =>: (C, B)

  ////
  val strongSyntax = new scalaz.syntax.StrongSyntax[=>:] { def F = Strong.this }
}

object Strong {
  @inline def apply[F[_, _]](implicit F: Strong[F]): Strong[F] = F

  import Isomorphism._

  def fromIso[F[_, _], G[_, _]](D: F <~~> G)(implicit E: Strong[G]): Strong[F] =
    new IsomorphismStrong[F, G] {
      override def G: Strong[G] = E
      override def iso: F <~~> G = D
    }

  ////

  ////
}

trait IsomorphismStrong[F[_, _], G[_, _]] extends Strong[F] with IsomorphismProfunctor[F, G]{
  implicit def G: Strong[G]
  ////

  override def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)] =
    iso.from(G.first(iso.to(fa)))

  override def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)] =
    iso.from(G.second(iso.to(fa)))
  ////
}
