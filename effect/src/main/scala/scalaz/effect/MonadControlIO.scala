package scalaz
package effect

////
/**
 *
 */
////
trait MonadControlIO[F[_]] extends LiftControlIO[F] with Monad[F] { self =>
  ////

  // derived functions

  ////
  val monadControlIOSyntax: scalaz.syntax.effect.MonadControlIOSyntax[F] =
    new scalaz.syntax.effect.MonadControlIOSyntax[F] { def F = MonadControlIO.this }
}

object MonadControlIO {
  @inline def apply[F[_]](implicit F: MonadControlIO[F]): MonadControlIO[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: MonadControlIO[G]): MonadControlIO[F] =
    new IsomorphismMonadControlIO[F, G] {
      override def G: MonadControlIO[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismMonadControlIO[F[_], G[_]] extends MonadControlIO[F] with IsomorphismLiftControlIO[F, G] with IsomorphismMonad[F, G]{
  implicit def G: MonadControlIO[G]
  ////
  ////
}
