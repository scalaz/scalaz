package scalaz

trait States {
  type State[S,A] = StateT[S,Id,A] 
  def apply[S,A](f: S => (A,S)): State[S,A] = new StateT[S,Id,A] {
    def apply(s: S) = f(s)
  }
  def init[S]: State[S,S] = State(s => (s,s)) 
  def put[S](s: S): State[S,S] = State(_ => (s,s))
  def modify[S](f: S => S): State[S,S] = State(s => { val r = f(s); (r,r) })

  implicit def state[S]: MonadStateInstance[({type f[s,a]=State[s,a]})#f, S] = 
    stateTInstance[S,Id](Id.id)
  implicit def stateT[S,F[_]:Monad] = stateTInstance[S,F]: 
    MonadState[({ type f[s,a]=StateT[s,F,a] })#f, S] with 
    Monad[({ type f[a]=StateT[S,F,a] })#f] with 
    Applicative[({ type f[a]=StateT[S,F,a] })#f] with 
    Apply[({ type f[a]=StateT[S,F,a] })#f] with 
    Bind[({ type f[a]=StateT[S,F,a] })#f]

  private def stateTInstance[S,F[_]](implicit F: Monad[F]) = 
    new MonadStateInstance[({type f[s,a]=StateT[s,F,a]})#f,S] {
      def pure[A](a: => A): StateT[S,F,A] = StateT(s => F.pure(a,s)) 
      override def map[A,B](fa: StateT[S,F,A])(f: A => B): StateT[S,F,B] = 
        StateT(s => F.map(fa(s)) { case (a,s) => (f(a),s) } )
      def bind[A,B](fa: StateT[S,F,A])(f: A => StateT[S,F,B]): StateT[S,F,B] =
        StateT(s => F.bind(fa(s)) { case (a,s) => f(a)(s) })
      def init: StateT[S,F,S] = StateT(s => F.pure((s,s)))
      def put(s: S): StateT[S,F,S] = StateT(_ => F.pure((s,s)))
    }
  trait StateTF[S,G[_]] { type f[x] = StateT[S,G,x] }

  implicit def StateMonadTrans[S] = new MonadTrans[({type f[g[_],a]=StateT[S,g,a]})#f] {
    def liftM[G[_],A](ga: G[A])(implicit G:Monad[G]): StateT[S,G,A] = 
      StateT(s => G.map(ga)(a => (a,s)))
    def hoist[M[_],N[_]](f: M ~> N) = new (StateTF[S,M]#f ~> StateTF[S,N]#f) {
      def apply[A](action: StateT[S,M,A]) = StateT[S,N,A](s => f(action(s)))
    }
  }
}
trait StateTs {
  def apply[S,F[_],A](f: S => F[(A,S)]): StateT[S,F,A] = new StateT[S,F,A] {
    def apply(s: S) = f(s)
  }
}

trait StateT[S,F[_],A] { 
  def apply(s: S): F[(A,S)]
  def !(s: S)(implicit F: Functor[F]): F[A] = 
    F.map(apply(s))(_._1)
}

object State extends States
object StateT extends StateTs
