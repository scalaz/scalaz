package scalaz

////
/**
 *
 */
////
trait CovariantDerives[F[_]] extends Derives[F] with Coapplicative[F] with Applicative[F] { self =>
  ////

  ////
  val covariantDerivesSyntax = new scalaz.syntax.CovariantDerivesSyntax[F] { def F = CovariantDerives.this }
}

object CovariantDerives {
  @inline def apply[F[_]](implicit F: CovariantDerives[F]): CovariantDerives[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: CovariantDerives[G]): CovariantDerives[F] =
    new IsomorphismCovariantDerives[F, G] {
      override def G: CovariantDerives[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismCovariantDerives[F[_], G[_]] extends CovariantDerives[F] with IsomorphismDerives[F, G] with IsomorphismCoapplicative[F, G] with IsomorphismApplicative[F, G]{
  implicit def G: CovariantDerives[G]
  ////

  ////
}
