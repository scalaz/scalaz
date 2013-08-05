package scalaz

import Id._

/**
 * Represents a function `A => M[B]`.
 */
final case class Kleisli[M[_], A, B](run: A => M[B]) { self =>
  import Kleisli._

  /** alias for `andThen` */
  def >=>[C](k: Kleisli[M, B, C])(implicit b: Bind[M]): Kleisli[M, A, C] =  kleisli((a: A) => b.bind(this(a))(k(_)))

  def andThen[C](k: Kleisli[M, B, C])(implicit b: Bind[M]): Kleisli[M, A, C] = this >=> k

  def >==>[C](k: B => M[C])(implicit b: Bind[M]): Kleisli[M, A, C] = this >=> kleisli(k)

  def andThenK[C](k: B => M[C])(implicit b: Bind[M]): Kleisli[M, A, C] = this >==> k

  /** alias for `compose` */
  def <=<[C](k: Kleisli[M, C, A])(implicit b: Bind[M]): Kleisli[M, C, B] = k >=> this

  def compose[C](k: Kleisli[M, C, A])(implicit b: Bind[M]): Kleisli[M, C, B] = k >=> this

  def <==<[C](k: C => M[A])(implicit b: Bind[M]): Kleisli[M, C, B] = kleisli(k) >=> this

  def composeK[C](k: C => M[A])(implicit b: Bind[M]): Kleisli[M, C, B] = this <==< k

  def traverse[F[_]](f: F[A])(implicit M: Applicative[M], F: Traverse[F]): M[F[B]] =
    F.traverse(f)(Kleisli.this(_))

  def =<<(a: M[A])(implicit m: Bind[M]): M[B] = m.bind(a)(run)

  def map[C](f: B => C)(implicit M: Functor[M]): Kleisli[M, A, C] =
    kleisli(a => M.map(run(a))(f))

  def mapK[N[_], C](f: M[B] => N[C]): Kleisli[N, A, C] =
    kleisli(a => f(run(a)))

  def flatMapK[C](f: B => M[C])(implicit M: Bind[M]): Kleisli[M, A, C] =
    kleisli(a => M.bind(run(a))(f))

  def flatMap[C](f: B => Kleisli[M, A, C])(implicit M: Bind[M]): Kleisli[M, A, C] =
    kleisli((r: A) => M.bind[B, C](run(r))(((b: B) => f(b).run(r))))

  def lift[L[_]: Applicative]: Kleisli[({type λ[α]=L[M[α]]})#λ, A, B] =
    kleisli[({type λ[α]=L[M[α]]})#λ, A, B](a => Applicative[L].point(self(a)))

  import Liskov._
  def unlift[N[_], FF[_]](implicit M: Comonad[N], ev: this.type <~< Kleisli[({type λ[α] = N[FF[α]]})#λ, A, B]): Kleisli[FF, A, B] =
    kleisli[FF, A, B]{a => Comonad[N].copoint(ev(self) run a)}

  def unliftId[N[_]](implicit M: Comonad[N], ev: this.type <~< Kleisli[({type λ[α] = N[α]})#λ, A, B]): Reader[A, B] =
    unlift[N, Id]

  def rwst[W, S](implicit M: Functor[M], W: Monoid[W]): ReaderWriterStateT[M, A, W, S, B] = ReaderWriterStateT(
    (r, s) => M.map(self(r)) {
      b => (W.zero, b, s)
    }
  )

  def state(implicit M: Functor[M]): StateT[M, A, B] =
    StateT(a => M.map(run(a))((a, _)))

  def liftMK[T[_[_], _]](implicit T: MonadTrans[T], M: Monad[M]): Kleisli[({type l[a] = T[M, a]})#l, A, B] =
    mapK[({type l[a] = T[M, a]})#l, B](ma => T.liftM(ma))

  def local[AA](f: AA => A): Kleisli[M, AA, B] = kleisli(f andThen run)

  def endo(implicit M: Functor[M], ev: A >~> B): Endomorphic[({type λ[α, β] = Kleisli[M, α, β]})#λ, A] =
    Endomorphic[({type λ[α, β] = Kleisli[M, α, β]})#λ, A](map(ev.apply))
}

//
// Prioritized Implicits for type class instances
//
sealed abstract class KleisliInstances8 {
  implicit def kleisliFunctor[F[_], R](implicit F0: Functor[F]): Functor[({type λ[α] = Kleisli[F, R, α]})#λ] = new KleisliFunctor[F, R] {
    implicit def F: Functor[F] = F0
  }
}

sealed abstract class KleisliInstances7 extends KleisliInstances8 {

  implicit def kleisliApply[F[_], R](implicit F0: Apply[F]): Apply[({type λ[α] = Kleisli[F, R, α]})#λ] = new KleisliApply[F, R] {
    implicit def F: Apply[F] = F0
  }

  implicit def kleisliDistributive[F[_], R](implicit F0: Distributive[F]): Distributive[({type λ[α] = Kleisli[F, R, α]})#λ] = new KleisliDistributive[F, R] {
    implicit def F: Distributive[F] = F0
  }
}

sealed abstract class KleisliInstances6 extends KleisliInstances7 {
  implicit def kleisliApplicative[F[_], R](implicit F0: Applicative[F]): Applicative[({type λ[α] = Kleisli[F, R, α]})#λ] = new KleisliApplicative[F, R] {
    implicit def F: Applicative[F] = F0
  }
  implicit def kleisliPlus[F[_], A](implicit F0: Plus[F]): Plus[({type λ[α] = Kleisli[F, A, α]})#λ] = new KleisliPlus[F, A] {
    implicit def F = F0
  }
}

sealed abstract class KleisliInstances5 extends KleisliInstances6 {
  implicit def kleisliApplicativePlus[F[_], R](implicit F0: ApplicativePlus[F]): ApplicativePlus[({type λ[α] = Kleisli[F, R, α]})#λ] = new ApplicativePlus[({type λ[α] = Kleisli[F, R, α]})#λ] with KleisliApplicative[F, R] with KleisliPlusEmpty[F, R] {
    implicit def F: ApplicativePlus[F] = F0
  }
  implicit def kleisliSemigroup[F[_], A, B](implicit FB0: Semigroup[F[B]]): Semigroup[Kleisli[F, A, B]] = new KleisliSemigroup[F, A, B] {
    implicit def FB = FB0
  }
}

sealed abstract class KleisliInstances4 extends KleisliInstances5 {
  implicit def kleisliMonadPlus[F[_], A](implicit F0: MonadPlus[F]): MonadPlus[({type λ[α] = Kleisli[F, A, α]})#λ] = new KleisliMonadPlus[F, A] {
    implicit def F = F0
  }
}

sealed abstract class KleisliInstances3 extends KleisliInstances4 {
  implicit def kleisliMonadReader[F[_], R](implicit F0: Monad[F]): MonadReader[({type λ[α, β] = Kleisli[F, α, β]})#λ, R] = new KleisliMonadReader[F, R] {
    implicit def F: Monad[F] = F0
  }
}

sealed abstract class KleisliInstances2 extends KleisliInstances3 {
  implicit def kleisliIdFunctor[R]: Functor[({type λ[α] = Kleisli[Id, R, α]})#λ] = kleisliFunctor[Id, R]

}

sealed abstract class KleisliInstances1 extends KleisliInstances2 {
  implicit def kleisliIdApplicative[R]: Applicative[({type λ[α] = Kleisli[Id, R, α]})#λ] = kleisliApplicative[Id, R]

}
sealed abstract class KleisliInstances0 extends KleisliInstances1 {
  implicit def kleisliIdApply[R]: Apply[({type λ[α] = Kleisli[Id, R, α]})#λ] = kleisliApply[Id, R]
}

abstract class KleisliInstances extends KleisliInstances0 {
  implicit def kleisliArrow[F[_]](implicit F0: Monad[F]): Arrow[({type λ[α, β]=Kleisli[F, α, β]})#λ] = new KleisliArrow[F] {
    implicit def F: Monad[F] = F0
  }

  implicit def kleisliContravariant[F[_], A]: Contravariant[({type λ[α] = Kleisli[F, α, A]})#λ] = new KleisliContravariant[F, A] {}

  implicit def kleisliIdMonadReader[R]: MonadReader[({type λ[α, β] = Kleisli[Id, α, β]})#λ, R] = kleisliMonadReader[Id, R]

  implicit def kleisliMonoid[F[_], A, B](implicit FB0: Monoid[F[B]]): Monoid[Kleisli[F, A, B]] = new KleisliMonoid[F, A, B] {
    implicit def FB = FB0
  }
  implicit def kleisliPlusEmpty[F[_], A](implicit F0: PlusEmpty[F]): PlusEmpty[({type λ[α] = Kleisli[F, A, α]})#λ] = new KleisliPlusEmpty[F, A] {
    implicit def F = F0
  }
  implicit def kleisliMonadTrans[R]: Hoist[({type λ[α[_], β] = Kleisli[α, R, β]})#λ] = new KleisliHoist[R] {}
}

trait KleisliFunctions {
  /**Construct a Kleisli from a Function1 */
  def kleisli[M[_], A, B](f: A => M[B]): Kleisli[M, A, B] = Kleisli(f)

  /**Implicitly unwrap the Function1 represented by the Kleisli */
  implicit def kleisliFn[M[_], A, B](k: Kleisli[M, A, B]): A => M[B] = k.run

  /**Pure Kleisli arrow */
  def ask[M[_] : Monad, A]: Kleisli[M, A, A] = kleisli(a => Monad[M].point(a))

  def local[M[_] : Monad, A, R](f: R => R)(fa: Kleisli[M, R, A]): Kleisli[M, R, A] = fa local f
}

object Kleisli extends KleisliInstances with KleisliFunctions

//
// Implementation traits for type class instances
//

import Kleisli.kleisli

//
// * -> *
//

private[scalaz] trait KleisliFunctor[F[_], R] extends Functor[({type λ[α] = Kleisli[F, R, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: Kleisli[F, R, A])(f: A => B): Kleisli[F, R, B] = fa map f
}

private[scalaz] trait KleisliApply[F[_], R] extends Apply[({type λ[α] = Kleisli[F, R, α]})#λ] with KleisliFunctor[F, R] {
  implicit def F: Apply[F]
  override def ap[A, B](fa: => Kleisli[F, R, A])(f: => Kleisli[F, R, A => B]): Kleisli[F, R, B] = Kleisli[F, R, B](r => F.ap(fa(r))(f(r)))
}

private[scalaz] trait KleisliDistributive[F[_], R] extends Distributive[({type λ[α] = Kleisli[F, R, α]})#λ] with KleisliFunctor[F, R] {
  implicit def F: Distributive[F]

  override def distributeImpl[G[_]: Functor, A, B](a: G[A])(f: A => Kleisli[F, R, B]): Kleisli[F, R, G[B]] =
    Kleisli(r => F.distribute(a)(f(_) run r))
}

private[scalaz] trait KleisliApplicative[F[_], R] extends Applicative[({type λ[α] = Kleisli[F, R, α]})#λ] with KleisliApply[F, R] {
  implicit def F: Applicative[F]
  def point[A](a: => A): Kleisli[F, R, A] = kleisli((r: R) => F.point(a))
}

private[scalaz] trait KleisliMonad[F[_], R] extends Monad[({type λ[α] = Kleisli[F, R, α]})#λ] with KleisliApplicative[F, R] {
  implicit def F: Monad[F]
  def bind[A, B](fa: Kleisli[F, R, A])(f: A => Kleisli[F, R, B]): Kleisli[F, R, B] = fa flatMap f
}

private[scalaz] trait KleisliMonadReader[F[_], R] extends MonadReader[({type f[s, a] = Kleisli[F, s, a]})#f, R] with KleisliApplicative[F, R] with KleisliMonad[F, R] {
  implicit def F: Monad[F]

  def ask: Kleisli[F, R, R] = Kleisli[F, R, R](r => F.point(r))
  def local[A](f: R => R)(fa: Kleisli[F, R, A]): Kleisli[F, R, A] = Kleisli[F, R, A](r => fa.run(f(r)))
}

private[scalaz] trait KleisliHoist[R] extends Hoist[({type λ[α[_], β] = Kleisli[α, R, β]})#λ] {
  def hoist[M[_]: Monad, N[_]](f: M ~> N): ({type f[x] = Kleisli[M, R, x]})#f ~> ({type f[x] = Kleisli[N, R, x]})#f =
    new (({type f[x] = Kleisli[M, R, x]})#f ~> ({type f[x] = Kleisli[N, R, x]})#f) {
      def apply[A](m: Kleisli[M, R, A]): Kleisli[N, R, A] = Kleisli[N, R, A](r => f(m(r)))
    }

  def liftM[G[_] : Monad, A](a: G[A]): Kleisli[G, R, A] = Kleisli(_ => a)

  implicit def apply[G[_] : Monad]: Monad[({type λ[α] = Kleisli[G, R, α]})#λ] = Kleisli.kleisliMonadReader
}

private[scalaz] trait KleisliMonadPlus[F[_], R] extends MonadPlus[({type λ[α] = Kleisli[F, R, α]})#λ] with KleisliPlusEmpty[F, R] with KleisliMonad[F, R] {
  implicit def F: MonadPlus[F]
}

private[scalaz] trait KleisliContravariant[F[_], X] extends Contravariant[({type λ[α] = Kleisli[F, α, X]})#λ] {
  def contramap[A, B](fa: Kleisli[F, A, X])(f: B => A) = fa local f
}

//
// (* *) -> *
//

private[scalaz] trait KleisliArrow[F[_]]
  extends Arrow[({type λ[α, β] = Kleisli[F, α, β]})#λ]
  with Choice[({type λ[α, β] = Kleisli[F, α, β]})#λ] {

  implicit def F: Monad[F]

  def compose[A, B, C](bc: Kleisli[F, B, C], ab: Kleisli[F, A, B]): Kleisli[F, A, C] = ab >=> bc

  def id[A]: Kleisli[F, A, A] = kleisli(a => F.point(a))

  def arr[A, B](f: A => B): Kleisli[F, A, B] = kleisli(a => F.point(f(a)))

  def first[A, B, C](f: Kleisli[F, A, B]): Kleisli[F, (A, C), (B, C)] = kleisli[F, (A, C), (B, C)] {
    case (a, c) => F.map(f.run(a))((b: B) => (b, c))
  }

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

private[scalaz] trait KleisliSemigroup[F[_], A, B] extends Semigroup[Kleisli[F, A, B]] {
  implicit def FB: Semigroup[F[B]]
  def append(f1: Kleisli[F, A, B], f2: => Kleisli[F, A, B]) = Kleisli[F, A, B](a => FB.append(f1.run(a), f2.run(a)))
}

private[scalaz] trait KleisliMonoid[F[_], A, B] extends Monoid[Kleisli[F, A, B]] with KleisliSemigroup[F, A, B] {
  implicit def FB: Monoid[F[B]]
  def zero = Kleisli[F, A, B](a => FB.zero)
}

private[scalaz] trait KleisliPlus[F[_], A] extends Plus[({type λ[α]=Kleisli[F, A, α]})#λ] {
  implicit def F: Plus[F]
  def plus[B](f1: Kleisli[F, A, B], f2: => Kleisli[F, A, B]) = Kleisli[F, A, B](a => F.plus[B](f1.run(a), f2.run(a)))
}

private[scalaz] trait KleisliPlusEmpty[F[_], A] extends PlusEmpty[({type λ[α]=Kleisli[F, A, α]})#λ] with KleisliPlus[F, A] {
  implicit def F: PlusEmpty[F]
  def empty[B] = Kleisli[F, A, B](a => F.empty[B])
}
