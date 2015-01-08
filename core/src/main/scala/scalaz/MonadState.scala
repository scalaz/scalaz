package scalaz

////
/** The class of monads supporting the operations of
 * [[scalaz.State]].
 *
 */
////
trait MonadState[F[_, _], S] extends Monad[F[S, ?]] { self =>
  ////

  def state[A](a: A): F[S, A] = bind(init)(s => point(a))
  def constantState[A](a: A, s: => S): F[S, A] = bind(put(s))(_ => point(a))
  def init: F[S, S]
  def get: F[S, S]
  def gets[A](f: S => A): F[S, A] = bind(init)(s => point(f(s)))
  def put(s: S): F[S, Unit]
  def modify(f: S => S): F[S, Unit] = bind(init)(s => put(f(s)))

  ////
  
}

object MonadState {
  @inline def apply[F[_, _], S](implicit F: MonadState[F, S]): MonadState[F, S] = F

  ////

  ////
}
