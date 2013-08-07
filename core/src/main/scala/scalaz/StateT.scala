package scalaz

import Id._

trait IndexedStateT[F[_], -S1, S2, A] { self =>
  /** Run and return the final value and state in the context of `F` */
  def apply(initial: S1): F[(S2, A)]

  /** An alias for `apply` */
  def run(initial: S1): F[(S2, A)] = apply(initial)

  /** Calls `run` using `Monoid[S].zero` as the initial state */
  def runZero[S <: S1](implicit S: Monoid[S]): F[(S2, A)] =
    run(S.zero)

  /** Run, discard the final state, and return the final value in the context of `F` */
  def eval(initial: S1)(implicit F: Functor[F]): F[A] =
    F.map(apply(initial))(_._2)

  /** Calls `eval` using `Monoid[S].zero` as the initial state */
  def evalZero[S <: S1](implicit F: Functor[F], S: Monoid[S]): F[A] =
    eval(S.zero)

  /** Run, discard the final value, and return the final state in the context of `F` */
  def exec(initial: S1)(implicit F: Functor[F]): F[S2] =
    F.map(apply(initial))(_._1)

  /** Calls `exec` using `Monoid[S].zero` as the initial state */
  def execZero[S <: S1](implicit F: Functor[F], S: Monoid[S]): F[S2] =
    exec(S.zero)

  def map[B](f: A => B)(implicit F: Functor[F]): IndexedStateT[F, S1, S2, B] = IndexedStateT(s => F.map(apply(s)) {
    case (s1, a) => (s1, f(a))
  })

  def xmap[X1, X2](f: S2 => X1)(g: X2 => S1)(implicit F: Functor[F]): IndexedStateT[F, X2, X1, A] = IndexedStateT(s => F.map(apply(g(s))) {
    case (s1, a) => (f(s1), a)
  })

  import BijectionT._
  def bmap[X, S >: S2 <: S1](b: Bijection[S, X])(implicit F: Functor[F]): StateT[F, X, A] =
    xmap(b to _)(b from _)

  def contramap[X](g: X => S1): IndexedStateT[F, X, S2, A] =
    IndexedStateT(s => apply(g(s)))

  def imap[X](f: S2 => X)(implicit F: Functor[F]): IndexedStateT[F, S1, X, A] = IndexedStateT(s => F.map(apply(s)) {
    case (s1, a) => (f(s1), a)
  })

  def bimap[X, B](f: S2 => X)(g: A => B)(implicit F: Functor[F]): IndexedStateT[F, S1, X, B] = IndexedStateT(s => F.map(apply(s)) {
    case (s1, a) => (f(s1), g(a))
  })

  def leftMap[X](f: S2 => X)(implicit F: Functor[F]): IndexedStateT[F, S1, X, A] =
    imap(f)

  def flatMap[S3, B](f: A => IndexedStateT[F, S2, S3, B])(implicit F: Bind[F]): IndexedStateT[F, S1, S3, B] = IndexedStateT(s => F.bind(apply(s)) {
    case (s1, a) => f(a)(s1)
  })

  def lift[M[_]: Applicative]: IndexedStateT[({type λ[α]=M[F[α]]})#λ, S1, S2, A] = new IndexedStateT[({type λ[α]=M[F[α]]})#λ, S1, S2, A] {
    def apply(initial: S1): M[F[(S2, A)]] = Applicative[M].point(self(initial))
  }

  import Liskov._
  def unlift[M[_], FF[_], S <: S1](implicit M: Comonad[M], ev: this.type <~< IndexedStateT[({type λ[α] = M[FF[α]]})#λ, S, S2, A]): IndexedStateT[FF, S, S2, A] = new IndexedStateT[FF, S, S2, A] {
    def apply(initial: S): FF[(S2, A)] = Comonad[M].copoint(ev(self)(initial))
  }

  def unliftId[M[_], S <: S1](implicit M: Comonad[M], ev: this.type <~< IndexedStateT[M, S, S2, A]): IndexedState[S, S2, A] = unlift[M, Id, S]

  def rwst[W, R](implicit F: Functor[F], W: Monoid[W]): IndexedReaderWriterStateT[F, R, W, S1, S2, A] = IndexedReaderWriterStateT(
    (r, s) => F.map(self(s)) {
      case (s, a) => (W.zero, a, s)
    }
  )

  def zoom[S0, S3, S <: S1](l: LensFamily[S0, S3, S, S2])(implicit F: Functor[F]): IndexedStateT[F, S0, S3, A] = new IndexedStateT[F, S0, S3, A] {
    def apply(s0: S0) = F.map(self(l get s0)) {
      case (s2, a) => (l.set(s0, s2), a)
    }
  }
}

object IndexedStateT extends StateTInstances with StateTFunctions {
  def apply[F[_], S1, S2, A](f: S1 => F[(S2, A)]): IndexedStateT[F, S1, S2, A] = new IndexedStateT[F, S1, S2, A] {
    def apply(s: S1) = f(s)
  }
}

//
// Prioritized Implicits for type class instances
//

sealed abstract class IndexedStateTInstances2 {
  implicit def indexedStateTContravariant[S2, A0, F[_]]: Contravariant[({type f[a] = IndexedStateT[F, a, S2, A0]})#f] = new IndexedStateTContravariant[S2, A0, F] {}
}

sealed abstract class IndexedStateTInstances1 extends IndexedStateTInstances2 {
  implicit def indexedStateTFunctorLeft[S1, A0, F[_]](implicit F0: Functor[F]): Functor[({type f[a] = IndexedStateT[F, S1, a, A0]})#f] = new IndexedStateTFunctorLeft[S1, A0, F] {
    implicit def F: Functor[F] = F0
  }
}

sealed abstract class IndexedStateTInstances0 extends IndexedStateTInstances1 {
  implicit def indexedStateTBifunctor[S1, F[_]](implicit F0: Functor[F]): Bifunctor[({type f[a, b] = IndexedStateT[F, S1, a, b]})#f] = new IndexedStateTBifunctor[S1, F] {
    implicit def F: Functor[F] = F0
  }
}

sealed abstract class IndexedStateTInstances extends IndexedStateTInstances0 {
  implicit def indexedStateTFunctorRight[S1, S2, F[_]](implicit F0: Functor[F]): Functor[({type f[a] = IndexedStateT[F, S1, S2, a]})#f] = new IndexedStateTFunctorRight[S1, S2, F] {
    implicit def F: Functor[F] = F0
  }
}

sealed abstract class StateTInstances1 extends IndexedStateTInstances {
  implicit def stateTMonadState[S, F[_]](implicit F0: Monad[F]): MonadState[({type f[s, a] = StateT[F, s, a]})#f, S] = new StateTMonadState[S, F] {
    implicit def F: Monad[F] = F0
  }
}

sealed abstract class StateTInstances0 extends StateTInstances1 {
  implicit def StateMonadTrans[S]: Hoist[({type f[g[_], a] = StateT[g, S, a]})#f] = new StateTHoist[S] {}
}

abstract class StateTInstances extends StateTInstances0 {
  implicit def stateMonad[S]: MonadState[({type f[s, a] = State[s, a]})#f, S] =
      StateT.stateTMonadState[S, Id](Id.id)
}

trait IndexedStateTFunctions {
  def constantIndexedStateT[F[_], S1, S2, A](a: A)(s: => S2)(implicit F: Applicative[F]): IndexedStateT[F, S1, S2, A] =
    IndexedStateT((_: S1) => F.point((s, a)))
}

trait StateTFunctions extends IndexedStateTFunctions {
  def constantStateT[F[_], S, A](a: A)(s: => S)(implicit F: Applicative[F]): StateT[F, S, A] =
    StateT((_: S) => F.point((s, a)))

  def stateT[F[_], S, A](a: A)(implicit F: Applicative[F]): StateT[F, S, A] =
    StateT(s => F.point((s, a)))
}

//
// Implementation traits for type class instances
//

private trait IndexedStateTContravariant[S2, A0, F[_]] extends Contravariant[({type f[a] = IndexedStateT[F, a, S2, A0]})#f] {
  override def contramap[A, B](fa: IndexedStateT[F, A, S2, A0])(f: B => A): IndexedStateT[F, B, S2, A0] = fa.contramap(f)
}

private trait IndexedStateTBifunctor[S1, F[_]] extends Bifunctor[({type f[a, b] = IndexedStateT[F, S1, a, b]})#f] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: IndexedStateT[F, S1, A, B])(f: A => C, g: B => D): IndexedStateT[F, S1, C, D] = fab.bimap(f)(g)
}

private trait IndexedStateTFunctorLeft[S1, A0, F[_]] extends Functor[({type f[a] = IndexedStateT[F, S1, a, A0]})#f] {
  implicit def F: Functor[F]

  override def map[A, B](fa: IndexedStateT[F, S1, A, A0])(f: A => B): IndexedStateT[F, S1, B, A0] = fa.imap(f)
}

private trait IndexedStateTFunctorRight[S1, S2, F[_]] extends Functor[({type f[a] = IndexedStateT[F, S1, S2, a]})#f] {
  implicit def F: Functor[F]

  override def map[A, B](fa: IndexedStateT[F, S1, S2, A])(f: A => B): IndexedStateT[F, S1, S2, B] = fa.map(f)
}

private trait StateTMonadState[S, F[_]] extends MonadState[({type f[s, a] = StateT[F, s, a]})#f, S] {
  implicit def F: Monad[F]

  def bind[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]): StateT[F, S, B] = fa.flatMap(f)

  def point[A](a: => A): StateT[F, S, A] = {
    lazy val aa = a
    StateT(s => F.point(s, aa))
  }

  def init: StateT[F, S, S] = StateT(s => F.point((s, s)))

  def get = init

  def put(s: S): StateT[F, S, Unit] = StateT(_ => F.point((s, ())))

  override def modify(f: S => S): StateT[F, S, Unit] = StateT(s => F.point((f(s), ())))

  override def gets[A](f: S => A): StateT[F, S, A] = StateT(s => F.point((s, f(s))))
}

private trait StateTHoist[S] extends Hoist[({type f[g[_], a] = StateT[g, S, a]})#f] {

  type StateTF[G[_], S] = {
    type f[x] = StateT[G, S, x]
  }

  def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]): StateT[G, S, A] =
    StateT(s => G.map(ga)(a => (s, a)))

  def hoist[M[_]: Monad, N[_]](f: M ~> N) = new (StateTF[M, S]#f ~> StateTF[N, S]#f) {
    def apply[A](action: StateT[M, S, A]) =
      StateT[N, S, A](s => f(action(s)))
  }

  implicit def apply[G[_] : Monad]: Monad[({type λ[α] = StateT[G, S, α]})#λ] = StateT.stateTMonadState[S, G]
}
