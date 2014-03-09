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
  val monadIOSyntax = new scalaz.syntax.effect.MonadIOSyntax[F] { def F = MonadIO.this }
}

object MonadIO {
  @inline def apply[F[_]](implicit F: MonadIO[F]): MonadIO[F] = F

  ////

  // TODO for some reason, putting this in RegionTInstances causes scalac to blow the stack
  implicit def regionTMonadIO[S, M[_]](implicit M0: MonadIO[M]) =
    new MonadIO[({type λ[α] = RegionT[S, M, α]})#λ] with RegionTLiftIO[S, M] with RegionTMonad[S, M] {
      implicit def M = M0
      implicit def L = M0
    }

  private[scalaz] trait FromLiftIO[F[_]] extends MonadIO[F] {
    def FM: Monad[F]
    def FLO: LiftIO[F]
    def point[A](a: => A) = FM.point(a)
    def bind[A, B](fa: F[A])(f: A => F[B]) = FM.bind(fa)(f)
    def liftIO[A](ioa: IO[A]) = FLO.liftIO(ioa)
  }

  private[scalaz] def fromLiftIO[F[_]: LiftIO: Monad]: MonadIO[F] = new FromLiftIO[F] {
    def FM = Monad[F]
    def FLO = LiftIO[F]
  }

  implicit def idTMonadIO[F[_]: MonadIO] = fromLiftIO[({type λ[α]=IdT[F, α]})#λ]

  implicit def listTMonadIO[F[_]: MonadIO] = fromLiftIO[({type λ[α]=ListT[F, α]})#λ]

  implicit def optionTMonadIO[F[_]: MonadIO] = fromLiftIO[({type λ[α]=OptionT[F, α]})#λ]

  implicit def eitherTMonadIO[F[_]: MonadIO, E] = fromLiftIO[({type λ[α]=EitherT[F, E, α]})#λ]

  implicit def streamTMonadIO[F[_]: MonadIO: Applicative] = fromLiftIO[({type λ[α]=StreamT[F, α]})#λ]

  implicit def kleisliMonadIO[F[_]: MonadIO, E] = fromLiftIO[({type λ[α]=Kleisli[F, E, α]})#λ]

  implicit def writerTMonadIO[F[_]: MonadIO, W: Monoid] = fromLiftIO[({type λ[α]=WriterT[F, W, α]})#λ]

  implicit def stateTMonadIO[F[_]: MonadIO, S] = fromLiftIO[({type λ[α]=StateT[F, S, α]})#λ]

  ////
}
