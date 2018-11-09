package scalaz

////
/**
 * Strength on a coproduct.
 */
////
trait ProChoice[=>:[_, _]] extends Profunctor[=>:] { self =>
  ////
  def left[A, B, C](fa: A =>: B): (A \/ C) =>: (B \/ C)

  def right[A, B, C](fa: A =>: B): (C \/ A) =>: (C \/ B)

  ////
  val proChoiceSyntax = new scalaz.syntax.ProChoiceSyntax[=>:] { def F = ProChoice.this }
}

object ProChoice {
  @inline def apply[F[_, _]](implicit F: ProChoice[F]): ProChoice[F] = F

  import Isomorphism._

  def fromIso[F[_, _], G[_, _]](D: F <~~> G)(implicit E: ProChoice[G]): ProChoice[F] =
    new IsomorphismProChoice[F, G] {
      override def G: ProChoice[G] = E
      override def iso: F <~~> G = D
    }

  ////

  ////
}

trait IsomorphismProChoice[F[_, _], G[_, _]] extends ProChoice[F] with IsomorphismProfunctor[F, G]{
  implicit def G: ProChoice[G]
  ////

  override def left[A, B, C](fa: F[A, B]): F[(A \/ C), (B \/ C)] =
    iso.from(G.left(iso.to(fa)))

  override def right[A, B, C](fa: F[A, B]): F[(C \/ A), (C \/ B)] =
    iso.from(G.right(iso.to(fa)))
  ////
}
