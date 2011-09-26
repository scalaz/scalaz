package scalaz

trait States0 {
  self: States =>
  implicit def stateTPointed[S,F[_]](implicit F0: Pointed[F]): Pointed[({ type f[a]=StateT[S,F,a] })#f] = new StateTPointed[S,F] {
    implicit def F: Pointed[F] = F0
  }
}
trait States1 {
  self: States =>
  implicit def stateTApplicative[S,F[_]](implicit F0: Applicative[F]): Applicative[({ type f[a]=StateT[S,F,a] })#f] = new StateTApplicative[S, F] {
    implicit def F: Applicative[F] = F0
  }
}

trait States extends States0 with States1 {
  type State[S,A] = StateT[S,Id,A] 
  def apply[S,A](f: S => (A,S)): State[S,A] = new StateT[S,Id,A] {
    def apply(s: S) = f(s)
  }
  def init[S]: State[S,S] = State(s => (s,s)) 
  def put[S](s: S): State[S,S] = State(_ => (s,s))
  def modify[S](f: S => S): State[S,S] = State(s => { val r = f(s); (r,r) })

  implicit def state[S]: MonadState[({type f[s,a]=State[s,a]})#f, S] =
    stateTMonadState[S,Id](Id.id)
  
  implicit def stateTMonadState[S,F[_]](implicit F0: Monad[F]) = new StateTMonadState[S,F] {
    implicit def F: Monad[F] = F0
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

trait StateTPointed[S, F[_]] extends Pointed[({type f[a] = StateT[S, F, a]})#f] {
 implicit def F: Pointed[F]

 def pure[A](a: => A): StateT[S, F, A] = StateT(s => F.pure(a, s))

 override def map[A, B](fa: StateT[S, F, A])(f: A => B): StateT[S, F, B] =
   StateT(s => F.map(fa(s)) {
     case (a, s) => (f(a), s)
   })
}

trait StateTApplicative[S, F[_]] extends Applicative[({type f[a] = StateT[S, F, a]})#f] with StateTPointed[S, F] {
 implicit def F: Applicative[F]
 override def ap[A, B](fa: StateT[S, F, A])(f: StateT[S, F, (A) => B]): StateT[S, F, B] = StateT {
     s =>
       F.ap(fa(s)) {
         F.map(f(s)) {
           case (fab, _) => {
             case (a, s) => (fab(a), s)
           }
         }
       }
   }
}

trait StateTMonadState[S, F[_]] extends MonadState[({type f[s,a]=StateT[s,F,a]})#f,S] with StateTApplicative[S, F] {
 implicit def F: Monad[F]

 def bind[A, B](fa: StateT[S, F, A])(f: A => StateT[S, F, B]): StateT[S, F, B] =
   StateT(s => F.bind(fa(s)) {
     case (a, s) => f(a)(s)
   })

 def init: StateT[S, F, S] = StateT(s => F.pure((s, s)))

 def put(s: S): StateT[S, F, S] = StateT(_ => F.pure((s, s)))
}
