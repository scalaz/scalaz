package scalaz
package effect

import Kleisli._

trait LiftIO[F[_]] {
  def liftIO[A]: IO[A] => F[A]
}

object LiftIO extends LiftIOs

trait LiftIOs {

  implicit val IOLiftIO: LiftIO[IO] = new LiftIO[IO] {
    def liftIO[A] = a => a
  }

  implicit def ReaderLiftIO[M[_], R](implicit lio: LiftIO[M]):
  LiftIO[({type λ[α] = Kleisli[R, M, α]})#λ] = {
    new LiftIO[({type λ[α] = Kleisli[R, M, α]})#λ] {
      def liftIO[A] = a => kleisli(_ => lio.liftIO(a))
    }
  }

  implicit def RegionTLiftIO[M[_], S](implicit lio: LiftIO[M]):
  LiftIO[({type λ[α] = RegionT[S, M, α]})#λ] = {
    new LiftIO[({type λ[α] = RegionT[S, M, α]})#λ] {
      def liftIO[A] = a => RegionT.regionT(kleisli(_ => lio.liftIO(a)))
    }
  }
}