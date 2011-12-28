package scalaz

/**
 * StateT Monad Transformer
 *
 * [[http://www.youtube.com/watch?feature=player_detailpage&v=XVmhK8WbRLY#t=585s An introduction to the State Monad]]
 */
trait StateT[F[_], S, A] { self =>
  /** Run and return the final value and state in the context of `F` */
  def apply(initial: S): F[(A, S)]

  /** An alias for `apply` */
  def run(initial: S): F[(A, S)] = apply(initial)

  /** Run, discard the final state, and return the final value in the context of `F` */
  def eval(initial: S)(implicit F: Functor[F]): F[A] =
    F.map(apply(initial))(_._1)

  /** Run, discard the final value, and return the final state in the context of `F` */
  def exec(initial: S)(implicit F: Functor[F]): F[S] =
    F.map(apply(initial))(_._2)

  def map[B](f: A => B)(implicit F: Functor[F]): StateT[F, S, B] = StateT(s => F.map(apply(s)) {
    case (a, s1) => (f(a), s1)
  })

  def flatMap[B](f: A => StateT[F, S, B])(implicit F: Bind[F]): StateT[F, S, B] = StateT(s => F.bind(apply(s)) {
    case (a, s1) => f(a)(s1)
  })

  def lift[M[_]: Pointed]: StateT[({type λ[α]=M[F[α]]})#λ, S, A] = new StateT[({type λ[α]=M[F[α]]})#λ, S, A] {
    def apply(initial: S): M[F[(A, S)]] = Pointed[M].point(self(initial))
  }

  import Liskov._
  def unlift[M[_], FF[_]](implicit M: CoPointed[M], ev: this.type <~< StateT[({type λ[α] = M[FF[α]]})#λ, S, A]): StateT[FF, S, A] = new StateT[FF, S, A] {
    def apply(initial: S): FF[(A, S)] = CoPointed[M].copoint(ev(self)(initial))
  }

  def unliftId[M[_]](implicit M: CoPointed[M], ev: this.type <~< StateT[({type λ[α] = M[α]})#λ, S, A]): State[S, A] = unlift[M, Id]

  def rwst[W, R](implicit F: Functor[F], W: Monoid[W]): ReaderWriterStateT[F, R, W, S, A] = ReaderWriterStateT(
    (r, s) => F.map(self(s)) {
      case (a, s) => (W.zero, a, s)
    }
  )
}

object StateT extends StateTFunctions with StateTInstances {
  def apply[F[_], S, A](f: S => F[(A, S)]): StateT[F, S, A] = new StateT[F, S, A] {
    def apply(s: S) = f(s)
  }
}

//
// Prioritized Implicits for type class instances
//

trait StateTInstances2 {
  implicit def stateTFunctor[S, F[_]](implicit F0: Functor[F]): Functor[({type f[a] = StateT[F, S, a]})#f] = new StateTFunctor[S, F] {
    implicit def F: Functor[F] = F0
  }
}

trait StateTInstances1 extends StateTInstances2 {
  implicit def stateTPointed[S, F[_]](implicit F0: Pointed[F]): Pointed[({type f[a] = StateT[F, S, a]})#f] = new StateTPointed[S, F] {
    implicit def F: Pointed[F] = F0
  }
}

trait StateTInstances0 extends StateTInstances1 {
  implicit def stateTMonadState[S, F[_]](implicit F0: Monad[F]): MonadState[({type f[s, a] = StateT[F, s, a]})#f, S] = new StateTMonadState[S, F] {
    implicit def F: Monad[F] = F0
  }

  implicit def StateMonadTrans[S]: MonadTrans[({type f[g[_], a] = StateT[g, S, a]})#f] = new StateTMonadTrans[S] {}
}

trait StateTInstances extends StateTInstances0 {
  implicit def stateMonad[S]: MonadState[({type f[s, a] = State[s, a]})#f, S] =
      StateT.stateTMonadState[S, Id](Id.id)
}

trait StateTFunctions {
  def constantStateT[F[_], A, S](a: A)(s: => S)(implicit F: Pointed[F]): StateT[F, S, A] =
    StateT((_: S) => F.point((a, s)))

  def stateT[F[_], A, S](a: A)(implicit F: Pointed[F]): StateT[F, S, A] =
    StateT(s => F.point((a, s)))
}

//
// Implementation traits for type class instances
//

private[scalaz] trait StateTFunctor[S, F[_]] extends Functor[({type f[a] = StateT[F, S, a]})#f] {
  implicit def F: Functor[F]

  override def map[A, B](fa: StateT[F, S, A])(f: A => B): StateT[F, S, B] = fa.map(f)
}

private[scalaz] trait StateTPointed[S, F[_]] extends Pointed[({type f[a] = StateT[F, S, a]})#f] with StateTFunctor[S, F] {
  implicit def F: Pointed[F]

  def point[A](a: => A): StateT[F, S, A] = {
    lazy val aa = a
    StateT(s => F.point(aa, s))
  }
}

private[scalaz] trait StateTMonadState[S, F[_]] extends MonadState[({type f[s, a] = StateT[F, s, a]})#f, S] with StateTPointed[S, F] {
  implicit def F: Monad[F]

  def bind[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]): StateT[F, S, B] = fa.flatMap(f)

  def init: StateT[F, S, S] = StateT(s => F.point((s, s)))

  def put(s: S): StateT[F, S, Unit] = StateT(_ => F.point(((), s)))
}

private[scalaz] trait StateTMonadTrans[S] extends MonadTrans[({type f[g[_], a] = StateT[g, S, a]})#f] {

  trait StateTF[G[_], S] {
    type f[x] = StateT[G, S, x]
  }

  def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]): StateT[G, S, A] =
    StateT(s => G.map(ga)(a => (a, s)))

  def hoist[M[_]: Monad, N[_]](f: M ~> N) = new (StateTF[M, S]#f ~> StateTF[N, S]#f) {
    def apply[A](action: StateT[M, S, A]) =
      StateT[N, S, A](s => f(action(s)))
  }
}
