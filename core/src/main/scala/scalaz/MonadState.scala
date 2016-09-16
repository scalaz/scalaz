package scalaz

////
/** The class of monads supporting the operations of
 * [[scalaz.State]].
 *
 */
////
trait MonadState[F[_], S] {
  ////
  def monad: Monad[F]

  def state[A](a: A): F[A] = monad.bind(init)(s => monad.point(a))
  def constantState[A](a: A, s: => S): F[A] = monad.bind(put(s))(_ => monad.point(a))
  def init: F[S]
  def get: F[S]
  def gets[A](f: S => A): F[A] = monad.bind(init)(s => monad.point(f(s)))
  def put(s: S): F[Unit]
  def modify(f: S => S): F[Unit] = monad.bind(init)(s => put(f(s)))

  ////

}

object MonadState {
  @inline def apply[F[_], S](implicit F: MonadState[F, S]): MonadState[F, S] = F

  ////

  ////
}
