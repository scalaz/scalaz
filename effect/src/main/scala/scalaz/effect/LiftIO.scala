package scalaz
package effect

import Kleisli._

trait LiftIO[F[_]] {
  def liftIO[A](ioa: IO[A]): F[A]
}

object LiftIO extends LiftIOs

trait LiftIOs {

  implicit val IOLiftIO: LiftIO[IO] = new LiftIO[IO] {
    def liftIO[A](ioa: IO[A]) = ioa
  }

  implicit def ReaderLiftIO[M[_], R](implicit lio: LiftIO[M]):
  LiftIO[({type λ[α] = Kleisli[M, R, α]})#λ] = {
    new LiftIO[({type λ[α] = Kleisli[M, R, α]})#λ] {
      def liftIO[A](ioa: IO[A]) = kleisli(_ => lio.liftIO(ioa))
    }
  }

  implicit def RegionTLiftIO[M[_], S](implicit lio: LiftIO[M]):
  LiftIO[({type λ[α] = RegionT[S, M, α]})#λ] = {
    new LiftIO[({type λ[α] = RegionT[S, M, α]})#λ] {
      def liftIO[A](ioa: IO[A]) = RegionT.regionT(kleisli(_ => lio.liftIO(ioa)))
    }
  }

  implicit def StateTLiftIO[S, F[_]](implicit lio: LiftIO[F], m: Monad[F]): LiftIO[({type λ[α] = StateT[S, F, α]})#λ] = 
    new LiftIO[({type λ[α] = StateT[S, F, α]})#λ] {
      def liftIO[A](ioa: IO[A]) = implicitly[MonadTrans[({type λ[α[_], β] = StateT[S, α, β]})#λ]].liftM(lio.liftIO(ioa))
    }
}