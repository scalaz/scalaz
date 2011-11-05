package scalaz

/**
 * StateT Monad Transformer
 *
 * [[http://www.youtube.com/watch?feature=player_detailpage&v=XVmhK8WbRLY#t=585s An introduction to the State Monad]]
 */
trait StateT[S, F[_], A] {
  /** Run and return the final value and state in the context of `F` */
  def apply(initial: S): F[(A, S)]

  /** Run, discard the final state, and return the final value in the context of `F` */
  def eval(initial: S)(implicit F: Functor[F]): F[A] =
    F.map(apply(initial))(_._1)

  /** Run, discard the final value, and return the final state in the context of `F` */
  def exec(initial: S)(implicit F: Functor[F]): F[S] =
    F.map(apply(initial))(_._2)

  def map[B](f: A => B)(implicit F: Functor[F]): StateT[S, F, B] = StateT(s => F.map(apply(s)) {
    case (a, s1) => (f(a), s1)
  })

  def flatMap[B](f: A => StateT[S, F, B])(implicit F: Bind[F]): StateT[S, F, B] = StateT(s => F.bind(apply(s)) {
    case (a, s1) => f(a)(s1)
  })
}

object StateT extends StateTFunctions with StateTInstances {
  def apply[S, F[_], A](f: S => F[(A, S)]): StateT[S, F, A] = new StateT[S, F, A] {
    def apply(s: S) = f(s)
  }
}

//
// Prioritized Implicits for type class instances
//

trait StateTInstances2 {
  implicit def stateTFunctor[S, F[_]](implicit F0: Functor[F]): Functor[({type f[a] = StateT[S, F, a]})#f] = new StateTFunctor[S, F] {
    implicit def F: Functor[F] = F0
  }
}

trait StateTInstances1 extends StateTInstances2 {
  implicit def stateTPointed[S, F[_]](implicit F0: Pointed[F]): Pointed[({type f[a] = StateT[S, F, a]})#f] = new StateTPointed[S, F] {
    implicit def F: Pointed[F] = F0
  }
}

trait StateTInstances extends StateTInstances1 {
  implicit def stateTMonadState[S, F[_]](implicit F0: Monad[F]): MonadState[({type f[s, a] = StateT[s, F, a]})#f, S] = new StateTMonadState[S, F] {
    implicit def F: Monad[F] = F0
  }

  implicit def StateMonadTrans[S]: MonadTrans[({type f[g[_], a] = StateT[S, g, a]})#f] = new StateTMonadTrans[S] {}
}

trait StateTFunctions {
  def constantStateT[F[_], A, S](a: A)(s: => S)(implicit F: Pointed[F]): StateT[S, F, A] =
    StateT((_: S) => F.point((a, s)))

  def stateT[F[_], A, S](a: A)(implicit F: Pointed[F]): StateT[S, F, A] =
    StateT(s => F.point((a, s)))
}

//
// Implementation traits for type class instances
//

private[scalaz] trait StateTFunctor[S, F[_]] extends Functor[({type f[a] = StateT[S, F, a]})#f] {
  implicit def F: Functor[F]

  override def map[A, B](fa: StateT[S, F, A])(f: A => B): StateT[S, F, B] = fa.map(f)
}

private[scalaz] trait StateTPointed[S, F[_]] extends StateTFunctor[S, F] with Pointed[({type f[a] = StateT[S, F, a]})#f] {
  implicit def F: Pointed[F]

  def point[A](a: => A): StateT[S, F, A] = StateT(s => F.point(a, s))
}

private[scalaz] trait StateTMonadState[S, F[_]] extends MonadState[({type f[s, a] = StateT[s, F, a]})#f, S] with StateTPointed[S, F] {
  implicit def F: Monad[F]

  def bind[A, B](fa: StateT[S, F, A])(f: A => StateT[S, F, B]): StateT[S, F, B] = fa.flatMap(f)

  def init: StateT[S, F, S] = StateT(s => F.point((s, s)))

  def put(s: S): StateT[S, F, Unit] = StateT(_ => F.point((s, s)))
}

private[scalaz] trait StateTMonadTrans[S] extends MonadTrans[({type f[g[_], a] = StateT[S, g, a]})#f] {

  trait StateTF[S, G[_]] {
    type f[x] = StateT[S, G, x]
  }

  def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]): StateT[S, G, A] =
    StateT(s => G.map(ga)(a => (a, s)))

  def hoist[M[_], N[_]](f: M ~> N) = new (StateTF[S, M]#f ~> StateTF[S, N]#f) {
    def apply[A](action: StateT[S, M, A]) = StateT[S, N, A](s => f(action(s)))
  }
}
