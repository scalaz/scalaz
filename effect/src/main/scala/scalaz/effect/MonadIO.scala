package scalaz
package effect

////
/**
 *
 */
////
trait MonadIO[F[_]] extends LiftIO[F] with Monad[F] { self =>
  ////

  // derived functions

  ////
  val monadIOSyntax = new scalaz.syntax.effect.MonadIOSyntax[F] {}
}

object MonadIO {
  @inline def apply[F[_]](implicit F: MonadIO[F]): MonadIO[F] = F

  ////
  implicit def KleisliMonadIO[R, M[_]](implicit M0: MonadIO[M]): MonadIO[({type λ[α] = Kleisli[M, R, α]})#λ] =
    new MonadIO[({type λ[α] = Kleisli[M, R, α]})#λ] with LiftIO.KleisliLiftIO[M, R] with KleisliMonad[M, R] {
      implicit def L = M0
      implicit def F = M0
    }

  implicit def StateTMonadIO[M[_], S](implicit M0: MonadIO[M]): MonadIO[({type λ[α] = StateT[M, S, α]})#λ] = {
    new MonadIO[({type λ[α] = StateT[M, S, α]})#λ] with LiftIO.StateTLiftIO[M, S] with StateTMonadState[S, M] {
      implicit def F = M0
      implicit def M = M0
    }
  }

  // TODO for some reason, putting this in RegionTInstances causes scalac to blow the stack
  implicit def regionTMonadIO[S, M[_]](implicit M0: MonadIO[M]) =
    new MonadIO[({type λ[α] = RegionT[S, M, α]})#λ] with RegionTLiftIO[S, M] with RegionTMonad[S, M] {
      implicit def M = M0
      implicit def L = M0
    }
  ////
}

