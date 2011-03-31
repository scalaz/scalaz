package scalaz
package effects

import Scalaz._

/** Classifies IO-like monads, enabling IO actions to be lifted into the monad. */
abstract class MonadIO[M[_]](implicit val value: Monad[M]) extends NewType[Monad[M]] {
  def liftIO[A](a: IO[A]): M[A]
}

/**
 * A class of IO-based monads supporting an extra operation liftControlIO, enabling conrol operations on IO
 * to be lifted into the monad.
 */
abstract class MonadControlIO[M[_]](implicit val value: MonadIO[M]) extends NewType[MonadIO[M]] {
  def liftControlIO[A](f: RunInBase[M, IO] => IO[A]): M[A]
}

object MonadIO {
  implicit val ioMonadIO: MonadIO[IO] = new MonadIO[IO] {
    def liftIO[A](a: IO[A]) = a
  }

  implicit def readerMonadIO[M[_], R](implicit mio: MonadIO[M]):
    MonadIO[({type λ[α] = Kleisli[M, R, α]})#λ] = {
      implicit val monad: Monad[M] = mio.value
      new MonadIO[({type λ[α] = Kleisli[M, R, α]})#λ] {
        def liftIO[A](a: IO[A]): Kleisli[M, R, A] = kleisli(r => mio.liftIO(a))
      }
  }

  implicit def regionTMonadIO[M[_], S](implicit mio: MonadIO[M]):
    MonadIO[({type λ[α] = RegionT[S, M, α]})#λ] = {
      implicit val monad: Monad[M] = mio.value
      new MonadIO[({type λ[α] = RegionT[S, M, α]})#λ] {
        def liftIO[A](a: IO[A]): RegionT[S, M, A] = RegionT(kleisli(r => mio.liftIO(a)))
      }
    }
}

