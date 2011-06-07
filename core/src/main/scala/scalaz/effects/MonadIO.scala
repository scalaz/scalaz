package scalaz
package effects

import Scalaz._

/** Classifies IO-like monads, enabling IO actions to be lifted into the monad. */
abstract class MonadIO[M[_]] {
  def liftControlIO[A](f: RunInBase[M, IO] => IO[A]): M[A]
  def liftIO[A](a: IO[A]): M[A]
}

abstract class MonadTransControl[T[_[_],_]] {
  import MonadTransControl._
  def liftControl[M[_]:Monad, A](f: Run[T] => M[A]): T[M, A]
  def lift[M[_]:Monad, A](m: M[A]): T[M, A]
  def join[M[_]:Monad, A](m: T[M, T[M, A]]): T[M, A]
}

object MonadTransControl {
  type Run[T[_[_],_]] = ForallM[Monad, ({type λ[n[_]] =
                        ForallM[Monad, ({type λ[o[_]] =
                          ForallK[Monad[({type λ[x] = T[o, x]})#λ],
                                  ({type λ[β] = T[n, β] => n[T[o, β]]})#λ]})#λ]})#λ]

  implicit def readerMonadTransControl[R]: MonadTransControl[({type λ[M[_], A] = Kleisli[M, R, A]})#λ] =
    new MonadTransControl[({type λ[M[_], A] = Kleisli[M, R, A]})#λ] {
      def liftControl[M[_]:Monad, A](f: Run[({type λ[N[_], A] = Kleisli[N, R, A]})#λ] => M[A]): Kleisli[M, R, A] =
        kleisli(r =>
          f(new Run[({type λ[N[_], A] = Kleisli[N, R, A]})#λ] {
            def apply[L[_]:Monad] =
              new ForallM[Monad, ({type λ[o[_]] =
                          ForallK[Monad[({type λ[x] = Kleisli[o, R, x]})#λ],
                                  ({type λ[β] = Kleisli[L, R, β] => L[Kleisli[o, R, β]]})#λ]})#λ] {
                def apply[O[_]: Monad] =
                  new ForallK[Monad[({type λ[x] = Kleisli[O, R, x]})#λ],
                              ({type λ[β] = Kleisli[L, R, β] => L[Kleisli[O, R, β]]})#λ] {
                    def apply[B](implicit m: Monad[({type λ[x] = Kleisli[O, R, x]})#λ]) = 
                      t => t(r).map(b => kleisli(_ => b.pure[O]))
                  }
              }
          })
        )
      def lift[M[_], A](m: M[A]): Kleisli[M, R, A] = kleisli(_ => m)
    }
}

object MonadIO {
  implicit val ioMonadIO: MonadIO[IO] = new MonadIO[IO] {
    def liftControlIO[A](f: RunInBase[IO, IO] => IO[A]): IO[A] =
      idLiftControl(f)
    def liftIO[A](a: IO[A]) = a
  }

  implicit def readerMonadIO[M[_], R](implicit mio: MonadIO[M], m: Monad[M]):
    MonadIO[({type λ[α] = Kleisli[M, R, α]})#λ] = 
      new MonadIO[({type λ[α] = Kleisli[M, R, α]})#λ] {
        def liftIO[A](a: IO[A]): Kleisli[M, R, A] = kleisli(r => mio.liftIO(a))
        def liftControlIO[A](f: RunInBase[({type λ[x] = Kleisli[M, R, x]})#λ, IO] => IO[A]): Kleisli[M, R, A] =
          liftLiftControlBase[M, IO, R, A](mio.liftControlIO, f)
      }

  implicit def regionMonadIO[M[_], S](implicit mio: MonadIO[M], m: Monad[M]):
    MonadIO[({type λ[α] = Region[S, M, α]})#λ] = 
      new MonadIO[({type λ[α] = Region[S, M, α]})#λ] {
        def liftIO[A](a: IO[A]): Region[S, M, A] = Region(kleisli(r => mio.liftIO(a)))
      }
}

