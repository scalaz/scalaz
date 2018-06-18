package scalaz

////
/**
 *
 */
////
trait Choice[=>:[_, _]] extends Category[=>:] { self =>
  ////
  def choice[A, B, C](f: => A =>: C, g: => B =>: C): (A \/ B) =>: C


  // derived functions

  def codiagonal[A]: (A \/ A) =>: A = choice(id, id)

  ////
  val choiceSyntax = new scalaz.syntax.ChoiceSyntax[=>:] { def F = Choice.this }
}

object Choice {
  @inline def apply[F[_, _]](implicit F: Choice[F]): Choice[F] = F

  import Isomorphism._

  def fromIso[F[_, _], G[_, _]](D: F <~~> G)(implicit E: Choice[G]): Choice[F] =
    new IsomorphismChoice[F, G] {
      override def G: Choice[G] = E
      override def iso: F <~~> G = D
    }

  ////
  ////
}

trait IsomorphismChoice[F[_, _], G[_, _]] extends Choice[F] with IsomorphismCategory[F, G]{
  implicit def G: Choice[G]
  ////

  override def choice[A, B, C](f: => F[A, C], g: => F[B, C]): F[(A \/ B), C] =
    iso.from(G.choice(iso.to(f), iso.to(g)))
  ////
}
