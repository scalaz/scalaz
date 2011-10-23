package scalaz
package effect

trait MonadIO[F[_]] extends LiftIO[F] with Monad[F] {
}

object MonadIO extends MonadIOs

trait MonadIOs {

/*  implicit val IOMonadIO: MonadIO[IO] =
    monadIO[IO]

  implicit def ReaderMonadIO[M[_], R](implicit mio: MonadIO[M]):
  MonadIO[({type λ[α] = Kleisli[R, M, α]})#λ] = {
    implicit val l = mio.liftIO
    implicit val m = mio.monad
    monadIO[({type λ[α] = Kleisli[R, M, α]})#λ]
  }

  implicit def RegionTMonadIO[M[_], S](implicit mio: MonadIO[M]):
  MonadIO[({type λ[α] = RegionT[S, M, α]})#λ] = {
    implicit val l = mio.liftIO
    implicit val m = mio.monad
    monadIO[({type λ[α] = RegionT[S, M, α]})#λ]
  }

  implicit def StateTMonadIO[S, M[_]](implicit mio: MonadIO[M]):
  MonadIO[({type λ[α] = StateT[S, M, α]})#λ] = {
    implicit val l = mio.liftIO
    implicit val m = mio.monad
    monadIO[({type λ[α] = StateT[S, M, α]})#λ]
  }*/
}