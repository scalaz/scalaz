package scalaz
package effect

////
/**
 *
 */
////
trait LiftIO[F[_]]  { self =>
  ////

  def liftIO[A](ioa: IO[A]): F[A]

  // derived functions

  ////
  val liftIOSyntax: scalaz.syntax.effect.LiftIOSyntax[F] =
    new scalaz.syntax.effect.LiftIOSyntax[F] { def F = LiftIO.this }
}

object LiftIO {
  @inline def apply[F[_]](implicit F: LiftIO[F]): LiftIO[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: LiftIO[G]): LiftIO[F] =
    new IsomorphismLiftIO[F, G] {
      override def G: LiftIO[G] = E
      override def iso: F <~> G = D
    }

  ////
  implicit def idTLiftIO[F[_]: LiftIO]: LiftIO[IdT[F, *]] =
    new LiftIO[IdT[F, *]] {
      def liftIO[A](ioa: IO[A]) = IdT(LiftIO[F].liftIO(ioa))
    }

  implicit def listTLiftIO[F[_]: LiftIO]: LiftIO[ListT[F, *]] =
    new LiftIO[ListT[F, *]] {
      def liftIO[A](ioa: IO[A]) = ListT(LiftIO[F].liftIO(ioa.map(_ :: INil())))
    }

  implicit def optionTLiftIO[F[_]: LiftIO]: LiftIO[OptionT[F, *]] =
    new LiftIO[OptionT[F, *]] {
      def liftIO[A](ioa: IO[A]) = OptionT(LiftIO[F].liftIO(ioa.map(Some(_): Option[A])))
    }

  implicit def eitherTLiftIO[F[_]: LiftIO, E]: LiftIO[EitherT[E, F, *]] =
    new LiftIO[EitherT[E, F, *]] {
      def liftIO[A](ioa: IO[A]) = EitherT(LiftIO[F].liftIO(ioa.map(\/.right)))
    }

  implicit def theseTLiftIO[F[_]: LiftIO, E]: LiftIO[TheseT[F, E, *]] =
    new LiftIO[TheseT[F, E, *]] {
      override def liftIO[A](ioa: IO[A]) = TheseT(LiftIO[F].liftIO(ioa.map(a => \&/.That(a))))
    }

  implicit def streamTLiftIO[F[_]: LiftIO: Applicative]: LiftIO[StreamT[F, *]] =
    new LiftIO[StreamT[F, *]] {
      def liftIO[A](ioa: IO[A]) =
        StreamT(LiftIO[F].liftIO(ioa.map(StreamT.Yield(_, StreamT.empty[F, A]))))
    }

  implicit def kleisliLiftIO[F[_]: LiftIO, E]: LiftIO[Kleisli[F, E, *]] =
    new LiftIO[Kleisli[F, E, *]] {
      def liftIO[A](ioa: IO[A]) = Kleisli(_ => LiftIO[F].liftIO(ioa))
    }

  implicit def writerTLiftIO[F[_]: LiftIO, W: Monoid]: LiftIO[WriterT[W, F, *]] =
    new LiftIO[WriterT[W, F, *]] {
      def liftIO[A](ioa: IO[A]) = WriterT(LiftIO[F].liftIO(ioa.map((Monoid[W].zero, _))))
    }

  implicit def stateTLiftIO[F[_]: LiftIO, S]: LiftIO[StateT[S, F, *]] =
    new LiftIO[StateT[S, F, *]] {
      def liftIO[A](ioa: IO[A]) = StateT(s => LiftIO[F].liftIO(ioa.map((s, _))))
    }

  ////
}

trait IsomorphismLiftIO[F[_], G[_]] extends LiftIO[F] {
  implicit def G: LiftIO[G]
  ////

  import Isomorphism._
  def iso: F <~> G

  override def liftIO[A](ioa: IO[A]): F[A] =
    iso.from(G.liftIO(ioa))
  ////
}
