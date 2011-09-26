package scalaz

trait MonadStateLike[F[_,_],S] extends Monad[({type f[x]=F[S,x]})#f] {
  def init: F[S,S]
  def put(s: S): F[S,S]
  def modify(f: S => S): F[S,S] = bind(init)(s => put(f(s)))
}

trait MonadState[F[_,_],S] extends MonadStateLike[F,S]

trait MonadStateInstance[F[_,_],S] extends MonadState[F,S] with MonadInstance[({type f[x]=F[S,x]})#f]
