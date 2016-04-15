package scalaz

import Id._

sealed abstract class IndexedStateT[F[_], -S1, S2, A] { self =>
  def getF[S <: S1]: Monad[F] => F[S => F[(S2, A)]]

  /** Run and return the final value and state in the context of `F` */
  def apply(initial: S1)(implicit F: Monad[F]): F[(S2, A)] =
    F.join(F.map[S1 => F[(S2, A)], F[(S2, A)]](getF(F))(sf => sf(initial)))

  /** An alias for `apply` */
  def run(initial: S1)(implicit F: Monad[F]): F[(S2, A)] = apply(initial)

  /** Calls `run` using `Monoid[S].zero` as the initial state */
  def runZero[S <: S1](implicit S: Monoid[S], F: Monad[F]): F[(S2, A)] =
    run(S.zero)

  /** Run, discard the final state, and return the final value in the context of `F` */
  def eval(initial: S1)(implicit F: Monad[F]): F[A] =
    F.bind[S1 => F[(S2, A)], A](getF(F))(sf => F.map(sf(initial))(_._2))

  /** Calls `eval` using `Monoid[S].zero` as the initial state */
  def evalZero[S <: S1](implicit F: Monad[F], S: Monoid[S]): F[A] =
    eval(S.zero)

  /** Run, discard the final value, and return the final state in the context of `F` */
  def exec(initial: S1)(implicit F: Monad[F]): F[S2] =
    F.map(run(initial))(_._1)

  /** Calls `exec` using `Monoid[S].zero` as the initial state */
  def execZero[S <: S1](implicit F: Monad[F], S: Monoid[S]): F[S2] =
    exec(S.zero)

  def map[B](f: A => B)(implicit F: Functor[F]): IndexedStateT[F, S1, S2, B] =
    mapsf((sf: (S1 => F[(S2, A)])) => (s: S1) => F.map(sf(s))(t => (t._1, f(t._2))))

  def xmap[X1, X2](f: S2 => X1)(g: X2 => S1): IndexedStateT[F, X2, X1, A] = IndexedStateT.createState(
    (F: Monad[F]) => (x: X2) => F.map(self(g(x))(F))(t => (f(t._1), t._2))
  )

  /** Map both the return value and final state using the given function. */
  def mapK[G[_], B, S](f: F[(S2, A)] => G[(S, B)])(implicit M: Monad[F]): IndexedStateT[G, S1, S, B] = IndexedStateT.createState(
    (m: Monad[G]) => (s: S1) => f(apply(s)(M))
  )

  import BijectionT._
  def bmap[X, S >: S2 <: S1](b: Bijection[S, X]): StateT[F, X, A] =
    xmap(b to _)(b from _)

  def contramap[X](g: X => S1): IndexedStateT[F, X, S2, A] =
    mapsf(_ compose g)

  def imap[X](f: S2 => X)(implicit F: Functor[F]): IndexedStateT[F, S1, X, A] = bimap(f)(a => a)

  def bimap[X, B](f: S2 => X)(g: A => B)(implicit F: Functor[F]): IndexedStateT[F, S1, X, B] = mapsf(sf => (s: S1) => F.map(sf(s))(t => (f(t._1), g(t._2)) ))

  def leftMap[X](f: S2 => X)(implicit F: Functor[F]): IndexedStateT[F, S1, X, A] =
    imap(f)

  def flatMap[S3, B](f: A => IndexedStateT[F, S2, S3, B])(implicit F: Monad[F]): IndexedStateT[F, S1, S3, B] =
    mapsf(sf => (s: S1) => F.bind[(S2, A), (S3, B)](sf(s)){ t =>
      val sfb: F[(S2 => F[(S3, B)])] = f(t._2).getF(F)
      F.bind[S2 => F[(S3, B)], (S3, B)](sfb)(ff => ff(t._1))
    })

  def lift[M[_]](implicit F: Monad[F], M: Applicative[M]): IndexedStateT[λ[α => M[F[α]]], S1, S2, A] =
    IndexedStateT.createState[λ[α => M[F[α]]], S1, S2, A](
      (m: Monad[λ[α => M[F[α]]]]) => (s: S1) => M.point(self(s))
    )

  import Liskov._
  def unlift[M[_], FF[_], S <: S1](implicit M: Comonad[M], F: Monad[λ[α => M[FF[α]]]], ev: this.type <~< IndexedStateT[λ[α => M[FF[α]]], S, S2, A]): IndexedStateT[FF, S, S2, A] = IndexedStateT.createState(
    (m: Monad[FF]) => (s: S) => {
      M.copoint(ev(self)(s))
    }
  )

  def unliftId[M[_], S <: S1](implicit M: Comonad[M], F: Monad[M], ev: this.type <~< IndexedStateT[M, S, S2, A]): IndexedState[S, S2, A] = unlift[M, Id, S]

  def rwst[W, R](implicit F: Monad[F], W: Monoid[W]): IndexedReaderWriterStateT[F, R, W, S1, S2, A] =
    IndexedReaderWriterStateT(
      (r, s) => F.bind[S1 => F[(S2, A)], (W, A, S2)] (getF(F))((sf: (S1 => F[(S2, A)])) => F.map(sf(s)) {
        case (s, a) => (W.zero, a, s)
      })
    )

  def zoom[S0, S3, S <: S1](l: LensFamily[S0, S3, S, S2])(implicit F: Functor[F]): IndexedStateT[F, S0, S3, A] =
    mapsf(sf => (s0:S0) => F.map(sf(l get s0))(t => (l.set(s0, t._1), t._2)))

  def liftF[S <: S1](implicit F: Functor[IndexedStateT[F, S, S2, ?]]) =
    Free.liftF[IndexedStateT[F, S, S2, ?], A](self)

  def mapsf[X1, X2, B](f: (S1 => F[(S2, A)]) => (X1 => F[(X2, B)])): IndexedStateT[F, X1, X2, B] =
    IndexedStateT.createState((m: Monad[F]) => f((s:S1) => run(s)(m)))
}

object IndexedStateT extends StateTInstances with StateTFunctions {
  def apply[F[_], S1, S2, A](f: S1 => F[(S2, A)])(implicit F: Monad[F]): IndexedStateT[F, S1, S2, A] =
    new IndexedStateT[F, S1, S2, A] {
      override def getF[S <: S1] = (m: Monad[F]) => F.point(f)
    }

  def createState[F[_], S1, S2, A](f: Monad[F] => S1 => F[(S2, A)]): IndexedStateT[F, S1, S2, A] =
    new IndexedStateT[F, S1, S2, A] {
      override def getF[S <: S1] = (m: Monad[F]) => m.point(f(m))
    }
}

//
// Prioritized Implicits for type class instances
//

sealed abstract class IndexedStateTInstances2 {
  implicit def indexedStateTContravariant[S2, A0, F[_]]: Contravariant[IndexedStateT[F, ?, S2, A0]] =
    new IndexedStateTContravariant[S2, A0, F] {}
}

sealed abstract class IndexedStateTInstances1 extends IndexedStateTInstances2 {
  implicit def indexedStateTFunctorLeft[S1, A0, F[_]](implicit F0: Functor[F]): Functor[IndexedStateT[F, S1, ?, A0]] =
    new IndexedStateTFunctorLeft[S1, A0, F] {
      implicit def F: Functor[F] = F0
    }
}

sealed abstract class IndexedStateTInstances0 extends IndexedStateTInstances1 {
  implicit def indexedStateTBifunctor[S1, F[_]](implicit F0: Functor[F]): Bifunctor[IndexedStateT[F, S1, ?, ?]] =
    new IndexedStateTBifunctor[S1, F] {
      implicit def F: Functor[F] = F0
    }
}

sealed abstract class IndexedStateTInstances extends IndexedStateTInstances0 {
  implicit def indexedStateTFunctorRight[S1, S2, F[_]](implicit F0: Functor[F]): Functor[IndexedStateT[F, S1, S2, ?]] =
    new IndexedStateTFunctorRight[S1, S2, F] {
      implicit def F: Functor[F] = F0
    }

  implicit def indexedStateTPlus[F[_]: Monad: Plus, S1, S2]: Plus[IndexedStateT[F, S1, S2, ?]] =
    new IndexedStateTPlus[F, S1, S2] {
      def F = implicitly
      def G = implicitly
    }
}

sealed abstract class StateTInstances3 extends IndexedStateTInstances {
  implicit def stateTBindRec[S, F[_]](implicit F0: Monad[F], F1: BindRec[F]): BindRec[StateT[F, S, ?]] =
    new StateTBindRec[S, F] {
      implicit def F: Monad[F] = F0
      implicit def B: BindRec[F] = F1
    }
}

sealed abstract class StateTInstances2 extends StateTInstances3 {
  implicit def stateTMonadState[S, F[_]](implicit F0: Monad[F]): MonadState[StateT[F, S, ?], S] = 
    new StateTMonadState[S, F] {
      implicit def F: Monad[F] = F0
    }
}

sealed abstract class StateTInstances1 extends StateTInstances2 {
  implicit def stateTMonadPlus[S, F[_]](implicit F0: MonadPlus[F]): MonadPlus[StateT[F, S, ?]] =
    new StateTMonadStateMonadPlus[S, F] {
      implicit def F: MonadPlus[F] = F0
    }
}

sealed abstract class StateTInstances0 extends StateTInstances1 {
  implicit def StateMonadTrans[S]: Hoist[λ[(g[_], a) => StateT[g, S, a]]] =
    new StateTHoist[S] {}
}

abstract class StateTInstances extends StateTInstances0 {
  implicit def stateMonad[S]: MonadState[State[S, ?], S] =
      StateT.stateTMonadState[S, Id](Id.id)
}

trait IndexedStateTFunctions {
  def constantIndexedStateT[F[_], S1, S2, A](a: A)(s: => S2)(implicit F: Applicative[F]): IndexedStateT[F, S1, S2, A] =
    IndexedStateT.createState((m: Monad[F]) => (_: S1) => F.point((s, a)))
}

trait StateTFunctions extends IndexedStateTFunctions {
  def constantStateT[F[_], S, A](a: A)(s: => S)(implicit F: Monad[F]): StateT[F, S, A] =
    StateT((_: S) => F.point((s, a)))

  def stateT[F[_], S, A](a: A)(implicit F: Monad[F]): StateT[F, S, A] =
    StateT(s => F.point((s, a)))
}

//
// Implementation traits for type class instances
//

private trait IndexedStateTContravariant[S2, A0, F[_]] extends Contravariant[IndexedStateT[F, ?, S2, A0]] {
  override def contramap[A, B](fa: IndexedStateT[F, A, S2, A0])(f: B => A): IndexedStateT[F, B, S2, A0] = fa.contramap(f)
}

private trait IndexedStateTBifunctor[S1, F[_]] extends Bifunctor[IndexedStateT[F, S1, ?, ?]] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: IndexedStateT[F, S1, A, B])(f: A => C, g: B => D): IndexedStateT[F, S1, C, D] = fab.bimap(f)(g)
}

private trait IndexedStateTFunctorLeft[S1, A0, F[_]] extends Functor[IndexedStateT[F, S1, ?, A0]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: IndexedStateT[F, S1, A, A0])(f: A => B): IndexedStateT[F, S1, B, A0] = fa.imap(f)
}

private trait IndexedStateTFunctorRight[S1, S2, F[_]] extends Functor[IndexedStateT[F, S1, S2, ?]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: IndexedStateT[F, S1, S2, A])(f: A => B): IndexedStateT[F, S1, S2, B] = fa.map(f)
}

private trait StateTBind[S, F[_]] extends Bind[StateT[F, S, ?]] {
  implicit def F: Monad[F]

  override def map[A, B](fa: StateT[F, S, A])(f: A => B): StateT[F, S, B] = fa.map(f)

  def bind[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]): StateT[F, S, B] = fa.flatMap(f)
}

private trait StateTBindRec[S, F[_]] extends StateTBind[S, F] with BindRec[StateT[F, S, ?]] {
  implicit def F: Monad[F]
  implicit def B: BindRec[F]

  def tailrecM[A, B](f: A => StateT[F, S, A \/ B])(a: A): StateT[F, S, B] = {
    def go(t: (S, A)): F[(S, A) \/ (S, B)] = {
      F.map(f(t._2)(t._1)) { case (s, m) =>
        m match {
          case -\/(a0) => -\/((s, a0))
          case \/-(b) => \/-((s, b))
        }
      }
    }

    IndexedStateT(s => B.tailrecM(go)((s, a)))
  }
}

private trait StateTMonadState[S, F[_]] extends MonadState[StateT[F, S, ?], S] with StateTBind[S, F] {
  implicit def F: Monad[F]

  def point[A](a: => A): StateT[F, S, A] = {
    val aa = Need(a)
    StateT(s => F.point(s, aa.value))
  }

  def init: StateT[F, S, S] = StateT(s => F.point((s, s)))

  def get = init

  def put(s: S): StateT[F, S, Unit] = StateT(_ => F.point((s, ())))

  override def modify(f: S => S): StateT[F, S, Unit] = StateT(s => F.point((f(s), ())))

  override def gets[A](f: S => A): StateT[F, S, A] = StateT(s => F.point((s, f(s))))
}

private trait StateTHoist[S] extends Hoist[λ[(g[_], a) => StateT[g, S, a]]] {

  type StateTF[G[_], S] = {
    type f[x] = StateT[G, S, x]
  }

  def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]): StateT[G, S, A] =
    StateT(s => G.map(ga)(a => (s, a)))

  def hoist[M[_]: Monad, N[_]](f: M ~> N) = new (StateTF[M, S]#f ~> StateTF[N, S]#f) {
    def apply[A](action: StateT[M, S, A]) = IndexedStateT.createState(
      (n: Monad[N]) => (s: S) => f(action.run(s))
    )
  }

  implicit def apply[G[_] : Monad]: Monad[StateT[G, S, ?]] = StateT.stateTMonadState[S, G]
}

private trait IndexedStateTPlus[F[_], S1, S2] extends Plus[IndexedStateT[F, S1, S2, ?]] {
  implicit def F: Monad[F]
  implicit def G: Plus[F]
  override final def plus[A](a: IndexedStateT[F, S1, S2, A], b: => IndexedStateT[F, S1, S2, A]) =
    IndexedStateT(s => G.plus(a.run(s), b.run(s)))
}

private trait StateTMonadStateMonadPlus[S, F[_]] extends StateTMonadState[S, F] with StateTHoist[S] with MonadPlus[StateT[F, S, ?]] with IndexedStateTPlus[F, S, S] {
  implicit def F: MonadPlus[F]
  override final def G = F

  def empty[A]: StateT[F, S, A] = liftM[F, A](F.empty[A])
}
