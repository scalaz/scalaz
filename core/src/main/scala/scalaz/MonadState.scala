package scalaz

////
/** The class of monads supporting the operations of
 * [[scalaz.State]].
 *
 */
////
trait MonadState[F[_], S] extends Monad[F] { self =>
  ////

  def get: F[S]
  def put(s: S): F[Unit]

  def state[A](f: S => (S, A)): F[A] = bind(init)(s => f(s) match { case (s, a) => bind(put(s))(_ => point(a)) })
  def constantState[A](a: A, s: => S): F[A] = bind(put(s))(_ => point(a))
  def init: F[S] = get
  def gets[A](f: S => A): F[A] = bind(init)(s => point(f(s)))
  def modify(f: S => S): F[Unit] = bind(init)(s => put(f(s)))

  ////

}

object MonadState {
  @inline def apply[F[_], S](implicit F: MonadState[F, S]): MonadState[F, S] = F

  ////

  ////
}
