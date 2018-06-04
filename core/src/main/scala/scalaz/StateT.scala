package scalaz

import scala.annotation.tailrec
import Id._

sealed abstract class IndexedStateT[-S1, S2, F[_], A] { self =>
  import IndexedStateT._

  /** Run and return the final value and state in the context of `F` */
  @tailrec
  final def apply(initial: S1)(implicit F: Bind[F]): F[(S2, A)] =
    this match {
      case Wrap(f) => f(initial)
      case FlatMap(Wrap(f), g) => F.bind(f(initial)) { case (sx, x) => g(sx, x).run(sx) }
      case FlatMap(FlatMap(f, g), h) => f.flatMapS((sx, x) => g(sx, x).flatMapS(h)).apply(initial)
    }

  /** An alias for `apply` */
  def run(initial: S1)(implicit F: Bind[F]): F[(S2, A)] = apply(initial)

  /** Run and return the final value and state in the context of `F` */
  def runRec(initial: S1)(implicit F: BindRec[F]): F[(S2, A)] = {

    abstract class Eval {
      type S0
      val s0: S0
      val st: IndexedStateT[S0, S2, F, A]

      @tailrec
      final def step: F[Eval \/ (S2, A)] = st match {
        case Wrap(f) => F.map(f(s0))(\/.right)
        case FlatMap(Wrap(f), g) => F.map(f(s0)){ case (sx, x) => \/.left(Eval(g(sx, x), sx)) }
        case FlatMap(FlatMap(f, g), h) => Eval(f.flatMapS((sx, x) => g(sx, x).flatMapS(h)), s0).step
      }
    }

    object Eval {
      def apply[S](f: IndexedStateT[S, S2, F, A], s: S): Eval = new Eval {
        type S0 = S
        val s0 = s
        val st = f
      }
    }

    F.tailrecM(Eval(this, initial))(_.step)
  }

  /** Calls `run` using `Monoid[S].zero` as the initial state */
  def runZero[S <: S1](implicit S: Monoid[S], F: Bind[F]): F[(S2, A)] =
    run(S.zero)

  /** Calls `run` using `Monoid[S].zero` as the initial state */
  def runZeroRec[S <: S1](implicit S: Monoid[S], F: BindRec[F]): F[(S2, A)] =
    runRec(S.zero)

  /** Run, discard the final state, and return the final value in the context of `F` */
  def eval(initial: S1)(implicit F: Bind[F]): F[A] =
    F.map(run(initial))(_._2)

  /** Run, discard the final state, and return the final value in the context of `F` */
  def evalRec(initial: S1)(implicit F: BindRec[F]): F[A] =
    F.map(runRec(initial))(_._2)

  /** Calls `eval` using `Monoid[S].zero` as the initial state */
  def evalZero[S <: S1](implicit F: Bind[F], S: Monoid[S]): F[A] =
    eval(S.zero)

  /** Calls `eval` using `Monoid[S].zero` as the initial state */
  def evalZeroRec[S <: S1](implicit F: BindRec[F], S: Monoid[S]): F[A] =
    evalRec(S.zero)

  /** Run, discard the final value, and return the final state in the context of `F` */
  def exec(initial: S1)(implicit F: Bind[F]): F[S2] =
    F.map(run(initial))(_._1)

  /** Run, discard the final value, and return the final state in the context of `F` */
  def execRec(initial: S1)(implicit F: BindRec[F]): F[S2] =
    F.map(runRec(initial))(_._1)

  /** Calls `exec` using `Monoid[S].zero` as the initial state */
  def execZero[S <: S1](implicit F: Bind[F], S: Monoid[S]): F[S2] =
    exec(S.zero)

  /** Calls `exec` using `Monoid[S].zero` as the initial state */
  def execZeroRec[S <: S1](implicit F: BindRec[F], S: Monoid[S]): F[S2] =
    execRec(S.zero)

  def map[B](f: A => B)(implicit F: Applicative[F]): IndexedStateT[S1, S2, F, B] =
    flatMap(a => StateT[S2, F, B](s => F.point((s, f(a)))))

  def xmap[X1, X2](f: S2 => X1)(g: X2 => S1)(implicit F: Applicative[F]): IndexedStateT[X2, X1, F, A] =
    imap(f).contramap(g)

  /** Map both the return value and final state using the given function. */
  def mapT[G[_], B, S](f: F[(S2, A)] => G[(S, B)])(implicit M: Monad[F]): IndexedStateT[S1, S, G, B] =
    IndexedStateT(s => f(apply(s)))

  /** Alias for mapT */
  def mapK[G[_], B, S](f: F[(S2, A)] => G[(S, B)])(implicit M: Monad[F]): IndexedStateT[S1, S, G, B] = mapT(f)

  import BijectionT._
  def bmap[X, S >: S2 <: S1](b: Bijection[S, X])(implicit F: Applicative[F]): StateT[X, F, A] =
    xmap(b to)(b from)

  def contramap[X](g: X => S1)(implicit F: Applicative[F]): IndexedStateT[X, S2, F, A] =
    IndexedStateT((s: X) => F.point((g(s), ()))).flatMap(_ => this)

  def imap[X](f: S2 => X)(implicit F: Applicative[F]): IndexedStateT[S1, X, F, A] = bimap(f)(a => a)

  def bimap[X, B](f: S2 => X)(g: A => B)(implicit F: Applicative[F]): IndexedStateT[S1, X, F, B] =
    flatMap(a => IndexedStateT(s2 => F.point((f(s2), g(a)))))

  def leftMap[X](f: S2 => X)(implicit F: Applicative[F]): IndexedStateT[S1, X, F, A] =
    imap(f)

  def flatMap[S3, B](f: A => IndexedStateT[S2, S3, F, B]): IndexedStateT[S1, S3, F, B] =
    flatMapS((s2, a) => f(a))

  def lift[M[_]](implicit F: Bind[F], M: Applicative[M]): IndexedStateT[S1, S2, λ[α => M[F[α]]], A] =
    IndexedStateT[S1, S2, λ[α => M[F[α]]], A](s => M.point(self(s)))

  import Liskov._
  def unlift[M[_], FF[_], S <: S1](implicit M: Comonad[M], F: Bind[λ[α => M[FF[α]]]], ev: this.type <~< IndexedStateT[S, S2, λ[α => M[FF[α]]], A]): IndexedStateT[S, S2, FF, A] =
    IndexedStateT(s => M.copoint(ev(self)(s)))

  def unliftId[M[_], S <: S1](implicit M: Comonad[M], F: Bind[M], ev: this.type <~< IndexedStateT[S, S2, M, A]): IndexedState[S, S2, A] = unlift[M, Id, S]

  def rwst[W, R](implicit F: Bind[F], W: Monoid[W]): IndexedReaderWriterStateT[R, W, S1, S2, F, A] =
    IndexedReaderWriterStateT(
      (r, s) => F.map(run(s)) {
        case (s, a) => (W.zero, a, s)
      }
    )

  def zoom[S0, S3, S <: S1](l: LensFamily[S0, S3, S, S2])(implicit F: Applicative[F]): IndexedStateT[S0, S3, F, A] =
    IndexedStateT((s0: S0) => F.point((s0, ()))).flatMapS((s0, _) =>
      this.contramap(l get (_: S0)).imap[S3](l.set(s0, _))
    )

  def liftF[S <: S1]: Free[IndexedStateT[S, S2, F, ?], A] =
    Free.liftF[IndexedStateT[S, S2, F, ?], A](self)

  private def flatMapS[S3, B](f: (S2, A) => IndexedStateT[S2, S3, F, B]): IndexedStateT[S1, S3, F, B] =
    FlatMap(this, f)
}

object IndexedStateT extends StateTInstances with StateTFunctions {
  private final case class Wrap[F[_], S1, S2, A](run: S1 => F[(S2, A)]) extends IndexedStateT[S1, S2, F, A]
  private final case class FlatMap[F[_], S1, S2, S3, A, B](a: IndexedStateT[S1, S2, F, A], f: (S2, A) => IndexedStateT[S2, S3, F, B]) extends IndexedStateT[S1, S3, F, B]

  def apply[S1, S2, F[_], A](f: S1 => F[(S2, A)]): IndexedStateT[S1, S2, F, A] =
    Wrap(f)
}

//
// Prioritized Implicits for type class instances
//

sealed abstract class IndexedStateTInstances3 {
  implicit def indexedStateProfunctor[S2, F[_]](implicit F0: Applicative[F]): Profunctor[IndexedStateT[?, S2, F, ?]] =
    new Profunctor[IndexedStateT[?, S2, F, ?]] {
      def mapfst[S1, B, S3](fab: IndexedStateT[S1, S2, F, B])(f: S3 => S1): IndexedStateT[S3, S2, F, B] = fab.contramap(f)

      def mapsnd[S1, B, D](fab: IndexedStateT[S1, S2, F, B])(f: B => D): IndexedStateT[S1, S2, F, D] = fab.map(f)
    }
}

sealed abstract class IndexedStateTInstances2 extends IndexedStateTInstances3 {
  implicit def indexedStateTContravariant[S2, A0, F[_]](implicit F0: Applicative[F]): Contravariant[IndexedStateT[?, S2, F, A0]] =
    new IndexedStateTContravariant[S2, A0, F] {
      implicit def F = F0
    }
}

sealed abstract class IndexedStateTInstances1 extends IndexedStateTInstances2 {
  implicit def indexedStateTFunctorLeft[S1, A0, F[_]](implicit F0: Applicative[F]): Functor[IndexedStateT[S1, ?, F, A0]] =
    new IndexedStateTFunctorLeft[S1, A0, F] {
      implicit def F: Applicative[F] = F0
    }
}

sealed abstract class IndexedStateTInstances0 extends IndexedStateTInstances1 {
  implicit def indexedStateTBifunctor[S1, F[_]](implicit F0: Applicative[F]): Bifunctor[IndexedStateT[S1, ?, F, ?]] =
    new IndexedStateTBifunctor[S1, F] {
      implicit def F: Applicative[F] = F0
    }
}

sealed abstract class IndexedStateTInstances extends IndexedStateTInstances0 {
  implicit def indexedStateTFunctorRight[S1, S2, F[_]](implicit F0: Applicative[F]): Functor[IndexedStateT[S1, S2, F, ?]] =
    new IndexedStateTFunctorRight[S1, S2, F] {
      implicit def F: Applicative[F] = F0
    }

  implicit def indexedStateTPlus[F[_]: Bind: Plus, S1, S2]: Plus[IndexedStateT[S1, S2, F, ?]] =
    new IndexedStateTPlus[F, S1, S2] {
      def F = implicitly
      def G = implicitly
    }
}

sealed abstract class StateTInstances3 extends IndexedStateTInstances {
  implicit def stateTBindRec[S, F[_]](implicit F0: Applicative[F]): BindRec[StateT[S, F, ?]] =
    new StateTBindRec[S, F] {
      implicit def F: Applicative[F] = F0
    }

  implicit def stateTMonadError[S, F[_], E](implicit F0: MonadError[F, E]): MonadError[StateT[S, F, ?], E] =
    new StateTMonadError[S, F, E] {
      implicit def F: MonadError[F, E] = F0
    }
}

sealed abstract class StateTInstances2 extends StateTInstances3 {
  implicit def stateTMonadState[S, F[_]](implicit F0: Applicative[F]): MonadState[StateT[S, F, ?], S] =
    new StateTMonadState[S, F] {
      implicit def F: Applicative[F] = F0
    }
}

sealed abstract class StateTInstances1 extends StateTInstances2 {
  implicit def stateTMonadPlus[S, F[_]](implicit F0: MonadPlus[F]): MonadPlus[StateT[S, F, ?]] =
    new StateTMonadStateMonadPlus[S, F] {
      implicit def F: MonadPlus[F] = F0
    }
}

sealed abstract class StateTInstances0 extends StateTInstances1 {
  implicit def StateMonadTrans[S]: Hoist[λ[(g[_], a) => StateT[S, g, a]]] =
    new StateTHoist[S] {}

  implicit def stateComonad[S](implicit S: Monoid[S]): Comonad[State[S, ?]] =
    new Comonad[State[S, ?]] {
      override def copoint[A](fa: State[S, A]): A =
        fa.eval(S.zero)

      override def cojoin[A](fa: State[S, A]): State[S, State[S, A]] =
        State[S, State[S, A]](s => (
          fa.exec(s),
          State((t: S) => fa.run(S.append(s, t)))))

      override def map[A, B](fa: State[S, A])(f: A => B): State[S, B] =
        fa map f

      override def cobind[A, B](fa: State[S, A])(f: State[S, A] => B): State[S, B] =
        cojoin(fa).map(f)
    }
}

abstract class StateTInstances extends StateTInstances0 {
  implicit def stateMonad[S]: MonadState[State[S, ?], S] =
      StateT.stateTMonadState[S, Id](Id.id)
}

trait IndexedStateTFunctions {
  def constantIndexedStateT[S1, S2, F[_], A](a: A)(s: => S2)(implicit F: Applicative[F]): IndexedStateT[S1, S2, F, A] =
    IndexedStateT((_: S1) => F.point((s, a)))
}

trait StateTFunctions extends IndexedStateTFunctions {
  def constantStateT[S, F[_], A](a: A)(s: => S)(implicit F: Applicative[F]): StateT[S, F, A] =
    StateT((_: S) => F.point((s, a)))

  def stateT[S, F[_], A](a: A)(implicit F: Applicative[F]): StateT[S, F, A] =
    StateT(s => F.point((s, a)))
}

//
// Implementation traits for type class instances
//

private trait IndexedStateTContravariant[S2, A0, F[_]] extends Contravariant[IndexedStateT[?, S2, F, A0]] {
  implicit def F: Applicative[F]

  override def contramap[A, B](fa: IndexedStateT[A, S2, F, A0])(f: B => A): IndexedStateT[B, S2, F, A0] = fa.contramap(f)
}

private trait IndexedStateTBifunctor[S1, F[_]] extends Bifunctor[IndexedStateT[S1, ?, F, ?]] {
  implicit def F: Applicative[F]

  override def bimap[A, B, C, D](fab: IndexedStateT[S1, A, F, B])(f: A => C, g: B => D): IndexedStateT[S1, C, F, D] = fab.bimap(f)(g)
}

private trait IndexedStateTFunctorLeft[S1, A0, F[_]] extends Functor[IndexedStateT[S1, ?, F, A0]] {
  implicit def F: Applicative[F]

  override def map[A, B](fa: IndexedStateT[S1, A, F, A0])(f: A => B): IndexedStateT[S1, B, F, A0] = fa.imap(f)
}

private trait IndexedStateTFunctorRight[S1, S2, F[_]] extends Functor[IndexedStateT[S1, S2, F, ?]] {
  implicit def F: Applicative[F]

  override def map[A, B](fa: IndexedStateT[S1, S2, F, A])(f: A => B): IndexedStateT[S1, S2, F, B] = fa.map(f)
}

private trait StateTBind[S, F[_]] extends Bind[StateT[S, F, ?]] {
  implicit def F: Applicative[F]

  override def map[A, B](fa: StateT[S, F, A])(f: A => B): StateT[S, F, B] = fa.map(f)

  def bind[A, B](fa: StateT[S, F, A])(f: A => StateT[S, F, B]): StateT[S, F, B] = fa.flatMap(f)
}

private trait StateTBindRec[S, F[_]] extends StateTBind[S, F] with BindRec[StateT[S, F, ?]] {
  def tailrecM[A, B](a: A)(f: A => StateT[S, F, A \/ B]): StateT[S, F, B] = {
    f(a).flatMap(_ match {
      case -\/(a) => tailrecM(a)(f)
      case \/-(b) => StateT(s => F.point((s, b)))
    })
  }
}

private trait StateTMonadState[S, F[_]] extends MonadState[StateT[S, F, ?], S] with StateTBind[S, F] {
  implicit def F: Applicative[F]

  def point[A](a: => A): StateT[S, F, A] = {
    val aa = Need(a)
    StateT(s => F.point((s, aa.value)))
  }

  def get: StateT[S, F, S] = StateT(s => F.point((s, s)))

  def put(s: S): StateT[S, F, Unit] = StateT(_ => F.point((s, ())))

  override def modify(f: S => S): StateT[S, F, Unit] = StateT(s => F.point((f(s), ())))

  override def gets[A](f: S => A): StateT[S, F, A] = StateT(s => F.point((s, f(s))))
}

private trait StateTMonadError[S, F[_], E] extends MonadError[StateT[S, F, ?], E] {
  implicit def F: MonadError[F, E]

  override def raiseError[A](e: E): StateT[S, F, A] =
    StateT(_ => F.raiseError(e))

  override def handleError[A](fa: StateT[S, F, A])(f: (E) => StateT[S, F, A]): StateT[S, F, A] =
    StateT(s => F.handleError(fa(s))(f(_)(s)))

  override def bind[A, B](fa: StateT[S, F, A])(f: (A) => StateT[S, F, B]): StateT[S, F, B] =
    fa flatMap f

  override def point[A](a: => A): StateT[S, F, A] =
    StateT(s => F.point((s, a)))
}

private trait StateTHoist[S] extends Hoist[StateT[S, ?[_], ?]] {

  def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]): StateT[S, G, A] =
    StateT(s => G.map(ga)(a => (s, a)))

  def hoist[M[_]: Monad, N[_]](f: M ~> N) = λ[StateT[S, M, ?] ~> StateT[S, N, ?]](_ mapT f.apply)

  implicit def apply[G[_] : Monad]: Monad[StateT[S, G, ?]] = StateT.stateTMonadState[S, G]
}

private trait IndexedStateTPlus[F[_], S1, S2] extends Plus[IndexedStateT[S1, S2, F, ?]] {
  implicit def F: Bind[F]
  implicit def G: Plus[F]
  override final def plus[A](a: IndexedStateT[S1, S2, F, A], b: => IndexedStateT[S1, S2, F, A]) =
    IndexedStateT(s => G.plus(a.run(s), b.run(s)))
}

private trait StateTMonadStateMonadPlus[S, F[_]] extends StateTMonadState[S, F] with StateTHoist[S] with MonadPlus[StateT[S, F, ?]] with IndexedStateTPlus[F, S, S] {
  implicit def F: MonadPlus[F]
  override final def G = F

  def empty[A]: StateT[S, F, A] = liftM[F, A](F.empty[A])
}
