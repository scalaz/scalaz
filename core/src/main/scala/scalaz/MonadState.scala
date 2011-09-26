package scalaz

trait MonadState[F[_,_],S] extends Monad[({type f[x]=F[S,x]})#f] {
  def init: F[S,S]
  def put(s: S): F[S,S]
  def modify(f: S => S): F[S,S] = bind(init)(s => put(f(s)))
}
