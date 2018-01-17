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

  ////

  ////
}
