package scalaz

import Id._

/**
 * StateT Monad Transformer
 *
 * [[http://www.youtube.com/watch?feature=player_detailpage&v=XVmhK8WbRLY#t=585s An introduction to the State Monad]]
 */
// TODO
//dabblego: I have another, sec, but it will require Enum
//[06:57am] dabblego: can you please put a comment there while yer there?
//[06:57am] dabblego: \p -> if p then succ else id -- for a start
//[06:58am] dabblego: :t \f p -> evalStateT (StateT (\s -> do r <- f s; q <- p s; return (r, if q then succ s else s))) mempty
//[06:58am] lambdabot: forall s (m :: * -> *) a. (Monad m, Enum s, Monoid s) => (s -> m a) -> (s -> m Bool) -> m a
//
trait StateT[F[+_], S, +A] { self =>
  /** Run and return the final value and state in the context of `F` */
  def apply(initial: S): F[(S, A)]

  /** An alias for `apply` */
  def run(initial: S): F[(S, A)] = apply(initial)

  /** Run, discard the final state, and return the final value in the context of `F` */
  def eval(initial: S)(implicit F: Functor[F]): F[A] =
    F.map(apply(initial))(_._2)

  /** Calls `eval` using `Monoid[S].zero` as the initial state */
  def evalZero(implicit F: Functor[F], S: Monoid[S]): F[A] =
    eval(S.zero)

  /** Run, discard the final value, and return the final state in the context of `F` */
  def exec(initial: S)(implicit F: Functor[F]): F[S] =
    F.map(apply(initial))(_._1)

  /** Calls `exec` using `Monoid[S].zero` as the initial state */
  def execZero(implicit F: Functor[F], S: Monoid[S]): F[S] =
    exec(S.zero)

  def map[B](f: A => B)(implicit F: Functor[F]): StateT[F, S, B] = StateT(s => F.map(apply(s)) {
    case (s1, a) => (s1, f(a))
  })

  def flatMap[B](f: A => StateT[F, S, B])(implicit F: Bind[F]): StateT[F, S, B] = StateT(s => F.bind(apply(s)) {
    case (s1, a) => f(a)(s1)
  })

  def lift[M[_]: Pointed]: StateT[({type λ[+α]=M[F[α]]})#λ, S, A] = new StateT[({type λ[+α]=M[F[α]]})#λ, S, A] {
    def apply(initial: S): M[F[(S, A)]] = Pointed[M].point(self(initial))
  }

  import Liskov._
  def unlift[M[+_], FF[+_], AA >: A](implicit M: Copointed[M], ev: this.type <~< StateT[({type λ[+α] = M[FF[α]]})#λ, S, AA]): StateT[FF, S, AA] = new StateT[FF, S, AA] {
    def apply(initial: S): FF[(S, AA)] = Copointed[M].copoint(ev(self)(initial))
  }

  def unliftId[M[+_], AA >: A](implicit M: Copointed[M], ev: this.type <~< StateT[M, S, AA]): State[S, AA] = unlift[M, Id, AA]

  def rwst[W, R](implicit F: Functor[F], W: Monoid[W]): ReaderWriterStateT[F, R, W, S, A] = ReaderWriterStateT(
    (r, s) => F.map(self(s)) {
      case (s, a) => (W.zero, a, s)
    }
  )
}

object StateT extends StateTFunctions with StateTInstances {
  def apply[F[+_], S, A](f: S => F[(S, A)]): StateT[F, S, A] = new StateT[F, S, A] {
    def apply(s: S) = f(s)
  }
}

//
// Prioritized Implicits for type class instances
//

trait StateTInstances2 {
  implicit def stateTFunctor[S, F[+_]](implicit F0: Functor[F]): Functor[({type f[a] = StateT[F, S, a]})#f] = new StateTFunctor[S, F] {
    implicit def F: Functor[F] = F0
  }
}

trait StateTInstances1 extends StateTInstances2 {
  implicit def stateTPointed[S, F[+_]](implicit F0: Pointed[F]): Pointed[({type f[a] = StateT[F, S, a]})#f] = new StateTPointed[S, F] {
    implicit def F: Pointed[F] = F0
  }
}

trait StateTInstances0 extends StateTInstances1 {
  implicit def stateTMonadState[S, F[+_]](implicit F0: Monad[F]): MonadState[({type f[s, +a] = StateT[F, s, a]})#f, S] = new StateTMonadState[S, F] {
    implicit def F: Monad[F] = F0
  }

  implicit def StateMonadTrans[S]: Hoist[({type f[g[+_], +a] = StateT[g, S, a]})#f] = new StateTHoist[S] {}
}

trait StateTInstances extends StateTInstances0 {
  implicit def stateMonad[S]: MonadState[({type f[s, a] = State[s, a]})#f, S] =
      StateT.stateTMonadState[S, Id](Id.id)
}

trait StateTFunctions {
  def constantStateT[F[+_], S, A](a: A)(s: => S)(implicit F: Pointed[F]): StateT[F, S, A] =
    StateT((_: S) => F.point((s, a)))

  def stateT[F[+_], S, A](a: A)(implicit F: Pointed[F]): StateT[F, S, A] =
    StateT(s => F.point((s, a)))
}

//
// Implementation traits for type class instances
//

private[scalaz] trait StateTFunctor[S, F[+_]] extends Functor[({type f[+a] = StateT[F, S, a]})#f] {
  implicit def F: Functor[F]

  override def map[A, B](fa: StateT[F, S, A])(f: A => B): StateT[F, S, B] = fa.map(f)
}

private[scalaz] trait StateTPointed[S, F[+_]] extends Pointed[({type f[+a] = StateT[F, S, a]})#f] with StateTFunctor[S, F] {
  implicit def F: Pointed[F]

  def point[A](a: => A): StateT[F, S, A] = {
    lazy val aa = a
    StateT(s => F.point(s, aa))
  }
}

private[scalaz] trait StateTMonadState[S, F[+_]] extends MonadState[({type f[s, +a] = StateT[F, s, a]})#f, S] with StateTPointed[S, F] {
  implicit def F: Monad[F]

  def bind[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]): StateT[F, S, B] = fa.flatMap(f)

  def init: StateT[F, S, S] = StateT(s => F.point((s, s)))

  def put(s: S): StateT[F, S, Unit] = StateT(_ => F.point((s, ())))
}

private[scalaz] trait StateTHoist[S] extends Hoist[({type f[g[+_], +a] = StateT[g, S, a]})#f] {

  trait StateTF[G[+_], S] {
    type f[+x] = StateT[G, S, x]
  }

  def liftM[G[+_], A](ga: G[A])(implicit G: Monad[G]): StateT[G, S, A] =
    StateT(s => G.map(ga)(a => (s, a)))

  def hoist[M[+_]: Monad, N[+_]](f: M ~> N) = new (StateTF[M, S]#f ~> StateTF[N, S]#f) {
    def apply[A](action: StateT[M, S, A]) =
      StateT[N, S, A](s => f(action(s)))
  }

  implicit def apply[G[+_] : Monad]: Monad[({type λ[+α] = StateT[G, S, α]})#λ] = StateT.stateTMonadState[S, G]
}
