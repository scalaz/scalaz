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
  val monadIOSyntax: scalaz.syntax.effect.MonadIOSyntax[F] =
    new scalaz.syntax.effect.MonadIOSyntax[F] { def F = MonadIO.this }
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
  implicit def regionTMonadIO[S, M[_]](implicit M0: MonadIO[M]): MonadIO[RegionT[S, M, *]] =
    new MonadIO[RegionT[S, M, *]] with RegionTLiftIO[S, M] with RegionTMonad[S, M] {
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

  private[scalaz] def fromLiftIO[F[_]](l: LiftIO[F], m: Monad[F]): MonadIO[F] = new FromLiftIO[F] {
    override def FM = m
    override def FLO = l
  }

  implicit def idTMonadIO[F[_]: MonadIO]: MonadIO[IdT[F, *]] =
    fromLiftIO[IdT[F, *]](LiftIO.idTLiftIO, IdT.idTMonad)

  implicit def listTMonadIO[F[_]: MonadIO]: MonadIO[ListT[F, *]] =
    fromLiftIO[ListT[F, *]](LiftIO.listTLiftIO, ListT.listTMonadPlus)

  implicit def optionTMonadIO[F[_]: MonadIO]: MonadIO[OptionT[F, *]] =
    fromLiftIO[OptionT[F, *]](LiftIO.optionTLiftIO, OptionT.optionTMonadPlus)

  implicit def eitherTMonadIO[F[_]: MonadIO, E]: MonadIO[DisjunctionT[E, F, *]] =
    fromLiftIO[EitherT[E, F, *]](LiftIO.eitherTLiftIO, EitherT.eitherTMonadError)

  implicit def theseTMonadIO[F[_]: MonadIO, E: Semigroup]: MonadIO[TheseT[F, E, *]] =
    fromLiftIO[TheseT[F, E, *]](LiftIO.theseTLiftIO, TheseT.theseTMonad)

  implicit def streamTMonadIO[F[_]: MonadIO: Applicative]: MonadIO[StreamT[F, *]] =
    fromLiftIO[StreamT[F, *]](LiftIO.streamTLiftIO, StreamT.StreamTMonadPlus)

  implicit def kleisliMonadIO[F[_]: MonadIO, E]: MonadIO[Kleisli[F, E, *]] =
    fromLiftIO[Kleisli[F, E, *]](LiftIO.kleisliLiftIO, Kleisli.kleisliMonadReader)

  implicit def writerTMonadIO[F[_]: MonadIO, W: Monoid]: MonadIO[WriterT[W, F, *]] =
    fromLiftIO[WriterT[W, F, *]](LiftIO.writerTLiftIO, WriterT.writerTMonad)

  implicit def stateTMonadIO[F[_]: MonadIO, S]: MonadIO[StateT[S, F, *]] =
    fromLiftIO[StateT[S, F, *]](LiftIO.stateTLiftIO, IndexedStateT.stateTMonadState)

  ////
}

trait IsomorphismMonadIO[F[_], G[_]] extends MonadIO[F] with IsomorphismLiftIO[F, G] with IsomorphismMonad[F, G]{
  implicit def G: MonadIO[G]
  ////
  ////
}
