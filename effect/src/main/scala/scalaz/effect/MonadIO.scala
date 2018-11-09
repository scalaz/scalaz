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

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: MonadIO[G]): MonadIO[F] =
    new IsomorphismMonadIO[F, G] {
      override def G: MonadIO[G] = E
      override def iso: F <~> G = D
    }

  ////

  // TODO for some reason, putting this in RegionTInstances causes scalac to blow the stack
  implicit def regionTMonadIO[S, M[_]](implicit M0: MonadIO[M]) =
    new MonadIO[RegionT[S, M, ?]] with RegionTLiftIO[S, M] with RegionTMonad[S, M] {
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

  implicit def idTMonadIO[F[_]: MonadIO] = fromLiftIO[IdT[F, ?]]

  implicit def listTMonadIO[F[_]: MonadIO] = fromLiftIO[ListT[F, ?]]

  implicit def optionTMonadIO[F[_]: MonadIO] = fromLiftIO[OptionT[F, ?]]

  implicit def eitherTMonadIO[F[_]: MonadIO, E] = fromLiftIO[EitherT[E, F, ?]]

  implicit def theseTMonadIO[F[_]: MonadIO, E: Semigroup] = fromLiftIO[TheseT[F, E, ?]]

  implicit def streamTMonadIO[F[_]: MonadIO: Applicative] = fromLiftIO[StreamT[F, ?]]

  implicit def kleisliMonadIO[F[_]: MonadIO, E] = fromLiftIO[Kleisli[F, E, ?]]

  implicit def writerTMonadIO[F[_]: MonadIO, W: Monoid] = fromLiftIO[WriterT[W, F, ?]]

  implicit def stateTMonadIO[F[_]: MonadIO, S] = fromLiftIO[StateT[S, F, ?]]

  ////
}

trait IsomorphismMonadIO[F[_], G[_]] extends MonadIO[F] with IsomorphismLiftIO[F, G] with IsomorphismMonad[F, G]{
  implicit def G: MonadIO[G]
  ////
  ////
}
