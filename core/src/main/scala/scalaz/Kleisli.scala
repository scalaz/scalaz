package scalaz

import Id._

/**
 * Represents a function `A => M[B]`.
 */
final case class Kleisli[M[_], A, B](run: A => M[B]) { self =>
  import Kleisli._

  def dimap[C, D](f: C => A, g: B => D)(implicit b: Functor[M]): Kleisli[M, C, D] =
    Kleisli(c => b.map(run(f(c)))(g))

  /** alias for `andThen` */
  def >=>[C](k: Kleisli[M, B, C])(implicit b: Bind[M]): Kleisli[M, A, C] =  kleisli((a: A) => b.bind(this(a))(k.run))

  def andThen[C](k: Kleisli[M, B, C])(implicit b: Bind[M]): Kleisli[M, A, C] = this >=> k

  def >==>[C](k: B => M[C])(implicit b: Bind[M]): Kleisli[M, A, C] = this >=> kleisli(k)

  def andThenK[C](k: B => M[C])(implicit b: Bind[M]): Kleisli[M, A, C] = this >==> k

  /** alias for `compose` */
  def <=<[C](k: Kleisli[M, C, A])(implicit b: Bind[M]): Kleisli[M, C, B] = k >=> this

  def compose[C](k: Kleisli[M, C, A])(implicit b: Bind[M]): Kleisli[M, C, B] = k >=> this

  def <==<[C](k: C => M[A])(implicit b: Bind[M]): Kleisli[M, C, B] = kleisli(k) >=> this

  def composeK[C](k: C => M[A])(implicit b: Bind[M]): Kleisli[M, C, B] = this <==< k

  def traverse[F[_]](f: F[A])(implicit M: Applicative[M], F: Traverse[F]): M[F[B]] =
    F.traverse(f)(run)

  def =<<(a: M[A])(implicit m: Bind[M]): M[B] = m.bind(a)(run)

  def map[C](f: B => C)(implicit M: Functor[M]): Kleisli[M, A, C] =
    kleisli(a => M.map(run(a))(f))

  def mapT[N[_], C](f: M[B] => N[C]): Kleisli[N, A, C] =
    kleisli(run andThen f)

  /** alias for mapT */
  def mapK[N[_], C](f: M[B] => N[C]): Kleisli[N, A, C] =
    mapT(f)

  def flatMapK[C](f: B => M[C])(implicit M: Bind[M]): Kleisli[M, A, C] =
    kleisli(a => M.bind(run(a))(f))

  def flatMap[C](f: B => Kleisli[M, A, C])(implicit M: Bind[M]): Kleisli[M, A, C] =
    kleisli((r: A) => M.bind[B, C](run(r))(((b: B) => f(b).run(r))))

  def lift[L[_]: Applicative]: Kleisli[λ[α => L[M[α]]], A, B] =
    kleisli[λ[α => L[M[α]]], A, B](a => Applicative[L].point(self(a)))

  def transform[N[_]](f: M ~> N): Kleisli[N, A, B] =
    kleisli(a => f(run(a)))

  def lower(implicit M: Applicative[M]): Kleisli[M, A, M[B]] =
    Kleisli(a => M.pure(this(a)))

  import Liskov._
  def unlift[N[_], FF[_]](implicit M: Comonad[N], ev: this.type <~< Kleisli[λ[α => N[FF[α]]], A, B]): Kleisli[FF, A, B] =
    kleisli[FF, A, B]{a => Comonad[N].copoint(ev(self) run a)}

  def unliftId[N[_]](implicit M: Comonad[N], ev: this.type <~< Kleisli[N[?], A, B]): Reader[A, B] =
    unlift[N, Id]

  def rwst[W, S](implicit M: Functor[M], W: Monoid[W]): ReaderWriterStateT[M, A, W, S, B] =
    ReaderWriterStateT(
      (r, s) => M.map(self(r)) {
        b => (W.zero, b, s)
      }
    )

  def state(implicit M: Monad[M]): StateT[M, A, B] =
    StateT(a => M.map(run(a))((a, _)))

  def liftMK[T[_[_], _]](implicit T: MonadTrans[T], M: Monad[M]): Kleisli[T[M, ?], A, B] =
    mapK[T[M, ?], B](ma => T.liftM(ma))

  def local[AA](f: AA => A): Kleisli[M, AA, B] =
    kleisli(f andThen run)

  def endo(implicit M: Functor[M], ev: A >~> B): Endomorphic[Kleisli[M, ?, ?], A] =
    Endomorphic[Kleisli[M, ?, ?], A](map(ev.apply))

  def liftF(implicit F: Functor[Kleisli[M, A, ?]]) =
    Free.liftF[Kleisli[M, A, ?], B](self)

  def tap(implicit F: Applicative[M]): Kleisli[M, A, A] =
    Kleisli(a => F.apply2(run(a), F.point(a))((_, b) => b))

}

//
// Prioritized Implicits for type class instances
//

sealed abstract class KleisliInstances13 {
  implicit def kleisliFunctor[F[_], R](implicit F0: Functor[F]): Functor[Kleisli[F, R, ?]] =
    new KleisliFunctor[F, R] {
      implicit def F: Functor[F] = F0
    }
}

sealed abstract class KleisliInstances12 extends KleisliInstances13 {

  implicit def kleisliApply[F[_], R](implicit F0: Apply[F]): Apply[Kleisli[F, R, ?]] =
    new KleisliApply[F, R] {
      implicit def F: Apply[F] = F0
    }
}

sealed abstract class KleisliInstances11 extends KleisliInstances12 {
  implicit def kleisliDistributive[F[_], R](implicit F0: Distributive[F]): Distributive[Kleisli[F, R, ?]] =
    new KleisliDistributive[F, R] {
      implicit def F: Distributive[F] = F0
    }
}

sealed abstract class KleisliInstances10 extends KleisliInstances11 {
  implicit def kleisliBind[F[_], R](implicit F0: Bind[F]): Bind[Kleisli[F, R, ?]] =
    new KleisliBind[F, R] {
      def F = F0
    }
}

sealed abstract class KleisliInstances9 extends KleisliInstances10 {
  implicit def kleisliZip[F[_], R](implicit F: Zip[F]): Zip[Kleisli[F, R, ?]] =
    new Zip[Kleisli[F, R, ?]] {
      def zip[A, B](a: => Kleisli[F, R, A], b: => Kleisli[F, R, B]) =
        Kleisli(r => F.zip(a(r), b(r)))
    }
}

sealed abstract class KleisliInstances8 extends KleisliInstances9 {
  implicit def kleisliApplicative[F[_], R](implicit F0: Applicative[F]): Applicative[Kleisli[F, R, ?]] =
    new KleisliApplicative[F, R] {
      implicit def F: Applicative[F] = F0
    }

  implicit def kleisliPlus[F[_], A](implicit F0: Plus[F]): Plus[Kleisli[F, A, ?]] =
    new KleisliPlus[F, A] {
      implicit def F = F0
    }
}

sealed abstract class KleisliInstances7 extends KleisliInstances8 {
  implicit def kleisliBindRec[F[_], R](implicit F0: BindRec[F]): BindRec[Kleisli[F, R, ?]] =
    new KleisliBindRec[F, R] {
      implicit def F: BindRec[F] = F0
    }
}

sealed abstract class KleisliInstances6 extends KleisliInstances7 {
  implicit def kleisliApplicativePlus[F[_], R](implicit F0: ApplicativePlus[F]): ApplicativePlus[Kleisli[F, R, ?]] =
    new ApplicativePlus[Kleisli[F, R, ?]] with KleisliApplicative[F, R] with KleisliPlusEmpty[F, R] {
      implicit def F: ApplicativePlus[F] = F0
    }

  implicit def kleisliSemigroup[F[_], A, B](implicit FB0: Semigroup[F[B]]): Semigroup[Kleisli[F, A, B]] =
    new KleisliSemigroup[F, A, B] {
      implicit def FB = FB0
    }
}

sealed abstract class KleisliInstances5 extends KleisliInstances6 {
  implicit def kleisliMonadError[F[_], E, R](implicit F0: MonadError[F, E]): MonadError[Kleisli[F, R, ?], E] =
    new KleisliMonadError[F, E, R] {
      implicit def F = F0
    }
}

sealed abstract class KleisliInstances4 extends KleisliInstances5 {
  implicit def kleisliMonadPlus[F[_], A](implicit F0: MonadPlus[F]): MonadPlus[Kleisli[F, A, ?]] =
    new KleisliMonadPlus[F, A] {
      implicit def F = F0
    }
}

sealed abstract class KleisliInstances3 extends KleisliInstances4 {
  implicit def kleisliMonadReader[F[_], R](implicit F0: Monad[F]): MonadReader[Kleisli[F, R, ?], R] =
    new KleisliMonadReader[F, R] {
      implicit def F: Monad[F] = F0
    }
}

sealed abstract class KleisliInstances2 extends KleisliInstances3 {
  implicit def kleisliIdFunctor[R]: Functor[Kleisli[Id, R, ?]] =
    kleisliFunctor[Id, R]
}

sealed abstract class KleisliInstances1 extends KleisliInstances2 {
  implicit def kleisliIdApplicative[R]: Applicative[Kleisli[Id, R, ?]] =
    kleisliApplicative[Id, R]

  implicit def kleisliStrong[F[_]: Functor]: Strong[Kleisli[F, ?, ?]] =
    new KleisliStrong[F] {
      def F = implicitly
    }
}
sealed abstract class KleisliInstances0 extends KleisliInstances1 {
  implicit def kleisliIdApply[R]: Apply[Kleisli[Id, R, ?]] =
    kleisliApply[Id, R]

  implicit def kleisliProChoice[F[_]](implicit F0: Applicative[F]): ProChoice[Kleisli[F, ?, ?]] =
    new KleisliProChoice[F] {
      implicit def F = F0
    }

  implicit def kleisliCompose[F[_]](implicit F0: Bind[F]): Compose[Kleisli[F, ?, ?]] =
    new KleisliCompose[F] {
      implicit def F = F0
    }
}

abstract class KleisliInstances extends KleisliInstances0 {
  implicit def kleisliArrow[F[_]](implicit F0: Monad[F]): Arrow[Kleisli[F, ?, ?]] with Choice[Kleisli[F, ?, ?]] =
    new KleisliArrow[F] {
      implicit def F: Monad[F] = F0
    }

  implicit def kleisliContravariant[F[_], A]: Contravariant[Kleisli[F, ?, A]] =
    new KleisliContravariant[F, A] {}

  implicit def kleisliIdMonadReader[R]: MonadReader[Kleisli[Id, R, ?], R] =
    kleisliMonadReader[Id, R]

  implicit def kleisliMonoid[F[_], A, B](implicit FB0: Monoid[F[B]]): Monoid[Kleisli[F, A, B]] =
    new KleisliMonoid[F, A, B] {
      implicit def FB = FB0
    }

  implicit def kleisliPlusEmpty[F[_], A](implicit F0: PlusEmpty[F]): PlusEmpty[Kleisli[F, A, ?]] =
    new KleisliPlusEmpty[F, A] {
      implicit def F = F0
    }

  implicit def kleisliMonadTrans[R]: Hoist[λ[(α[_], β) => Kleisli[α, R, β]]] =
    new KleisliHoist[R] {}

  implicit def kleisliCatchable[F[_], A](implicit F0: Catchable[F]): Catchable[Kleisli[F, A, ?]] =
    new KleisliCatchable[F, A] {
      implicit def F = F0
    }
}

object Kleisli extends KleisliInstances {
  /**Construct a Kleisli from a Function1 */
  def kleisli[M[_], A, B](f: A => M[B]): Kleisli[M, A, B] =
    Kleisli(f)

  /** A version of `kleisli` that infers the type constructor `M`, when `M` is `Bind`
   * @example
   * {{{
   * Kleisli.kleisliU{s: String => try \/-(s.toInt) catch{ case e: NumberFormatException => -\/(e) }}
   * }}}
   */
  def kleisliU[A, MB](f: A => MB)(implicit MB: Unapply[Bind, MB]): Kleisli[MB.M, A, MB.A] =
    Kleisli(MB.leibniz.onF(f))

  /**Implicitly unwrap the Function1 represented by the Kleisli */
  implicit def kleisliFn[M[_], A, B](k: Kleisli[M, A, B]): A => M[B] =
    k.run

  /**Pure Kleisli arrow */
  def ask[M[_] : Applicative, A]: Kleisli[M, A, A] =
    kleisli(a => Applicative[M].point(a))

  def local[M[_], A, R](f: R => R)(fa: Kleisli[M, R, A]): Kleisli[M, R, A] =
    fa local f
}

//
// Implementation traits for type class instances
//

import Kleisli.kleisli

//
// * -> *
//

private trait KleisliFunctor[F[_], R] extends Functor[Kleisli[F, R, ?]] {
  implicit def F: Functor[F]
  override def map[A, B](fa: Kleisli[F, R, A])(f: A => B): Kleisli[F, R, B] =
    fa map f
}

private trait KleisliApply[F[_], R] extends Apply[Kleisli[F, R, ?]] with KleisliFunctor[F, R] {
  implicit def F: Apply[F]
  override def ap[A, B](fa: => Kleisli[F, R, A])(f: => Kleisli[F, R, A => B]): Kleisli[F, R, B] =
    Kleisli[F, R, B](r => F.ap(fa(r))(f(r)))
}

private trait KleisliDistributive[F[_], R] extends Distributive[Kleisli[F, R, ?]] with KleisliFunctor[F, R] {
  implicit def F: Distributive[F]

  override def distributeImpl[G[_]: Functor, A, B](a: G[A])(f: A => Kleisli[F, R, B]): Kleisli[F, R, G[B]] =
    Kleisli(r => F.distribute(a)(f(_) run r))
}

private trait KleisliBind[F[_], R] extends Bind[Kleisli[F, R, ?]] with KleisliApply[F, R] {
  implicit def F: Bind[F]
  override final def bind[A, B](fa: Kleisli[F, R, A])(f: A => Kleisli[F, R, B]) =
    fa flatMap f
}

private trait KleisliApplicative[F[_], R] extends Applicative[Kleisli[F, R, ?]] with KleisliApply[F, R] {
  implicit def F: Applicative[F]
  def point[A](a: => A): Kleisli[F, R, A] =
    kleisli((r: R) => F.point(a))
}

private trait KleisliBindRec[F[_], R] extends BindRec[Kleisli[F, R, ?]] with KleisliBind[F, R] {
  implicit def F: BindRec[F]

  def tailrecM[A, B](f: A => Kleisli[F, R, A \/ B])(a: A): Kleisli[F, R, B] =
    Kleisli(r => F.tailrecM(f(_: A).run(r))(a))
}

private trait KleisliMonad[F[_], R] extends Monad[Kleisli[F, R, ?]] with KleisliApplicative[F, R] with KleisliBind[F, R] {
  implicit def F: Monad[F]
}

private trait KleisliMonadReader[F[_], R] extends MonadReader[Kleisli[F, R, ?], R] with KleisliApplicative[F, R] with KleisliMonad[F, R] {
  implicit def F: Monad[F]

  def ask: Kleisli[F, R, R] =
    Kleisli[F, R, R](r => F.point(r))

  def local[A](f: R => R)(fa: Kleisli[F, R, A]): Kleisli[F, R, A] =
    fa.local(f)
}

private trait KleisliHoist[R] extends Hoist[Kleisli[?[_], R, ?]] {
  def hoist[M[_]: Monad, N[_]](f: M ~> N): Kleisli[M, R, ?] ~> Kleisli[N, R, ?] =
    new (Kleisli[M, R, ?] ~> Kleisli[N, R, ?]) {
      def apply[A](m: Kleisli[M, R, A]): Kleisli[N, R, A] =
        m.mapT(f)
    }

  def liftM[G[_] : Monad, A](a: G[A]): Kleisli[G, R, A] =
    Kleisli(_ => a)

  implicit def apply[G[_] : Monad]: Monad[Kleisli[G, R, ?]] =
    Kleisli.kleisliMonadReader
}

private trait KleisliMonadPlus[F[_], R] extends MonadPlus[Kleisli[F, R, ?]] with KleisliPlusEmpty[F, R] with KleisliMonad[F, R] {
  implicit def F: MonadPlus[F]
}

private trait KleisliMonadError[F[_], E, R] extends MonadError[Kleisli[F, R, ?], E] with KleisliMonad[F, R] {
  implicit def F: MonadError[F, E]

  def handleError[A](fa: Kleisli[F, R, A])(f: E => Kleisli[F, R, A]): Kleisli[F, R, A] =
    Kleisli.kleisli[F, R, A](r => F.handleError(fa.run(r))(e => f(e).run(r)))

  def raiseError[A](e: E): Kleisli[F, R, A] =
    Kleisli.kleisli[F, R, A](_ => F.raiseError(e))
}

private trait KleisliContravariant[F[_], X] extends Contravariant[Kleisli[F, ?, X]] {
  def contramap[A, B](fa: Kleisli[F, A, X])(f: B => A) = fa local f
}

//
// (* *) -> *
//
private trait KleisliStrong[F[_]] extends Strong[Kleisli[F, ?, ?]] {

  implicit def F: Functor[F]

  def first[A, B, C](f: Kleisli[F, A, B]): Kleisli[F, (A, C), (B, C)] =
    Kleisli {
      case (a, c) => F.map(f.run(a))((b: B) => (b, c))
    }

  def second[A, B, C](f: Kleisli[F, A, B]): Kleisli[F, (C, A), (C, B)] =
    Kleisli {
      case (c, a) => F.map(f.run(a))((b: B) => (c, b))
    }

  override def mapfst[A, B, C](fa: Kleisli[F, A, B])(f: C => A) = fa local f

  override def mapsnd[A, B, C](fa: Kleisli[F, A, B])(f: B => C) = fa map f
}

private trait KleisliProChoice[F[_]] extends ProChoice[Kleisli[F, ?, ?]] with KleisliStrong[F] {

  implicit def F: Applicative[F]

  def left[A, B, C](fa: Kleisli[F, A, B]): Kleisli[F, A \/ C, B \/ C] =
    Kleisli {
      case -\/(a) => F.map(fa run a)(\/.left)
      case b @ \/-(_) => F.point(b)
    }

  def right[A, B, C](fa: Kleisli[F, A, B]): Kleisli[F, C \/ A, C \/ B] =
    Kleisli {
      case b @ -\/(_) => F.point(b)
      case \/-(a) => F.map(fa run a)(\/.right)
    }
}

private trait KleisliCompose[F[_]] extends Compose[Kleisli[F, ?, ?]] {

  implicit def F: Bind[F]

  def compose[A, B, C](bc: Kleisli[F, B, C], ab: Kleisli[F, A, B]): Kleisli[F, A, C] = ab >=> bc
}

private trait KleisliArrow[F[_]]
  extends Arrow[Kleisli[F, ?, ?]]
  with Choice[Kleisli[F, ?, ?]]
  with KleisliCompose[F]
  with KleisliProChoice[F] {

  implicit def F: Monad[F]

  override def second[A, B, C](f: Kleisli[F, A, B]): Kleisli[F, (C, A), (C, B)] =
    super[KleisliProChoice].second(f)

  def id[A]: Kleisli[F, A, A] =
    kleisli(a => F.point(a))

  def arr[A, B](f: A => B): Kleisli[F, A, B] =
    kleisli(a => F.point(f(a)))

  def choice[A, B, C](f: => Kleisli[F, A, C], g: => Kleisli[F, B, C]): Kleisli[F, A \/ B, C] =
    Kleisli {
      case -\/(a) => f run a
      case \/-(b) => g run b
    }

  override def split[A, B, C, D](f: Kleisli[F, A, B], g: Kleisli[F, C, D]): Kleisli[F, (A, C), (B, D)] =
    Kleisli {
      case (a, c) =>
        F.bind(f run a)(b => F.map(g run c)(d => (b, d)))
    }
}

private trait KleisliSemigroup[F[_], A, B] extends Semigroup[Kleisli[F, A, B]] {
  implicit def FB: Semigroup[F[B]]

  def append(f1: Kleisli[F, A, B], f2: => Kleisli[F, A, B]) =
    Kleisli[F, A, B](a => FB.append(f1.run(a), f2.run(a)))
}

private trait KleisliMonoid[F[_], A, B] extends Monoid[Kleisli[F, A, B]] with KleisliSemigroup[F, A, B] {
  implicit def FB: Monoid[F[B]]

  def zero =
    Kleisli[F, A, B](a => FB.zero)
}

private trait KleisliPlus[F[_], A] extends Plus[Kleisli[F, A, ?]] {
  implicit def F: Plus[F]

  def plus[B](f1: Kleisli[F, A, B], f2: => Kleisli[F, A, B]) =
    Kleisli[F, A, B](a => F.plus[B](f1.run(a), f2.run(a)))
}

private trait KleisliPlusEmpty[F[_], A] extends PlusEmpty[Kleisli[F, A, ?]] with KleisliPlus[F, A] {
  implicit def F: PlusEmpty[F]

  def empty[B] =
    Kleisli[F, A, B](a => F.empty[B])
}

private trait KleisliCatchable[F[_], A] extends Catchable[Kleisli[F, A, ?]] {
  implicit def F: Catchable[F]

  def attempt[B](f: Kleisli[F, A, B]): Kleisli[F, A, Throwable \/ B] =
    Kleisli(a => F.attempt(try f.run(a) catch { case t: Throwable => F.fail(t) }))

  def fail[B](err: Throwable): Kleisli[F, A, B] =
    Kleisli(_ => F.fail(err))
}
