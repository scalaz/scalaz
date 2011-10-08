package scalaz

trait MonadState[F[_,_],S] extends Monad[({type f[x]=F[S,x]})#f] {
  def init: F[S,S]
  def put(s: S): F[S,Unit]
  def modify(f: S => S): F[S, Unit] = bind(init)(s => put(f(s)))
  def gets[A](f: S => A): F[S, A] = bind(init)(s => pure(f(s)))
}

object MonadState {
  def apply[F[_,_],S](implicit F: MonadState[F, S]) = F
}
