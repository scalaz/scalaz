package scalaz

/**
 * Represents a function `A => M[B]`.
 */
sealed trait Kleisli[M[_], A, B] {
  def run(a: A): M[B]

  import Kleisli._

  // TODO provide non-symbolic aliases.

  def >=>[C](k: Kleisli[M, B, C])(implicit b: Bind[M]): Kleisli[M, A, C] = kleisli((a: A) => b.bind(this(a))(k(_)))

  def >=>[C](k: B => M[C])(implicit b: Bind[M]): Kleisli[M, A, C] = >=>(kleisli(k))

  def <=<[C](k: Kleisli[M, C, A])(implicit b: Bind[M]): Kleisli[M, C, B] = k >=> this

  def <=<[C](k: C => M[A])(implicit b: Bind[M]): Kleisli[M, C, B] = kleisli(k) >=> this

  def compose[N[_]](f: M[B] => N[B]): Kleisli[N, A, B] = kleisli((a: A) => f(this(a)))

  def traverse[F[_], AA <: A](f: F[AA])(implicit M: Applicative[M], F: Traverse[F]): M[F[B]] =
    F.traverse(f)(Kleisli.this(_))

  def =<<[AA <: A](a: M[AA])(implicit m: Bind[M]): M[B] = m.bind(a)(run _)

  def map[C](f: B => C)(implicit m: Functor[M]): Kleisli[M, A, C] =
    kleisli(a => m.map(run(a))(f))

  def flatMap[C](f: B => M[C])(implicit M: Bind[M]): Kleisli[M, A, C] =
    kleisli(a => M.bind(run(a))(f))

  def flatMapK[C](f: B => Kleisli[M, A, C])(implicit M: Bind[M]): Kleisli[M, A, C] =
    kleisli((r: A) => M.bind[B, C](run(r))(((b: B) => f(b).run(r))))
}

//
// Prioritized Implicits for type class instances
//
trait KleisliInstances2 {
  implicit def kleisliFunctor[F[_], R](implicit F0: Functor[F]): Functor[({type λ[α] = Kleisli[F, R, α]})#λ] = new KleisliFunctor[F, R] {
    implicit def F: Functor[F] = F0
  }
}

trait KleisliInstances1 extends KleisliInstances2 {
  implicit def kleisliPointed[F[_], R](implicit F0: Pointed[F]): Pointed[({type λ[α] = Kleisli[F, R, α]})#λ] = new KleisliPointed[F, R] {
    implicit def F: Pointed[F] = F0
  }
  implicit def kleisliApply[F[_], R](implicit F0: Apply[F]): Apply[({type λ[α] = Kleisli[F, R, α]})#λ] = new KleisliApply[F, R] {
    implicit def F: Apply[F] = F0
  }
}

trait KleisliInstances0 extends KleisliInstances1 {
  implicit def kleisliApplicative[F[_], R](implicit F0: Applicative[F]): Applicative[({type λ[α] = Kleisli[F, R, α]})#λ] = new KleisliApplicative[F, R] {
    implicit def F: Applicative[F] = F0
  }

  implicit def kleisliArrId[F[_]](implicit F0: Pointed[F]) = new KleisliArrIdArr[F] {
    implicit def F: Pointed[F] = F0
  }
}

trait KleisliInstances extends KleisliInstances0 {
  implicit def kleisliCategory[F[_]](implicit F0: Monad[F]) = new KleisliCategory[F] {
    implicit def F: Monad[F] = F0
  }

  implicit def kleisliMonadReader[F[_], R](implicit F0: Monad[F]) = new KleisliMonadReader[F, R] {
    implicit def F: Monad[F] = F0
  }

  /** Kleisli version of `Monoid[Endo[A]]`. `append(f1, f2) == f1 <=< f2`. */
  implicit def kleisliMonoid[F[_], A](implicit F0: Monad[F]) = new Monoid[Kleisli[F, A, A]] {
    def append(f1: Kleisli[F, A, A], f2: => Kleisli[F, A, A]): Kleisli[F, A, A] = f1 <=< f2
    def zero: Kleisli[F, A, A] = Kleisli(a => F0.point(a))
  }
}

trait KleisliFunctions extends KleisliInstances0 {
  /**Construct a Kleisli from a Function1 */
  def kleisli[M[_], A, B](f: A => M[B]): Kleisli[M, A, B] = new Kleisli[M, A, B] {
    def run(a: A) = f(a)
  }

  /**Implicitly unwrap the Function1 represented by the Kleisli */
  implicit def kleisliFn[M[_], A, B](k: Kleisli[M, A, B]): A => M[B] = (a: A) => k.run(a)

  /**Pure Kleisli arrow */
  def ask[M[_] : Monad, A]: Kleisli[M, A, A] = kleisli(a => Monad[M].point(a))
}

object Kleisli extends KleisliFunctions with KleisliInstances {
  def apply[M[_], A, B](f: A => M[B]): Kleisli[M, A, B] = kleisli(f)
}

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

private[scalaz] trait KleisliPointed[F[_], R] extends Pointed[({type λ[α] = Kleisli[F, R, α]})#λ] with KleisliFunctor[F, R] {
  implicit def F: Pointed[F]

  def point[A](a: => A): Kleisli[F, R, A] = kleisli((r: R) => F.point(a))
}

private[scalaz] trait KleisliApply[F[_], R] extends Apply[({type λ[α] = Kleisli[F, R, α]})#λ] with KleisliFunctor[F, R] {
  implicit def F: Apply[F]
  override def ap[A, B](fa: => Kleisli[F, R, A])(f: => Kleisli[F, R, (A) => B]): Kleisli[F, R, B] = Kleisli[F, R, B](r => F.ap(fa(r))(f(r)))
}

private[scalaz] trait KleisliApplicative[F[_], R] extends Applicative[({type λ[α] = Kleisli[F, R, α]})#λ] with KleisliApply[F, R] with KleisliPointed[F, R]{
  implicit def F: Applicative[F]
}

private[scalaz] trait KleisliMonadReader[F[_], R] extends MonadReader[({type f[s, a] = Kleisli[F, s, a]})#f, R] with KleisliApplicative[F, R] {
  implicit def F: Monad[F]

  override def bind[A, B](fa: Kleisli[F, R, A])(f: A => Kleisli[F, R, B]): Kleisli[F, R, B] = fa flatMapK f
  def ask: Kleisli[F, R, R] = Kleisli[F, R, R](r => F.point(r))
  def local[A](f: (R) => R)(fa: Kleisli[F, R, A]): Kleisli[F, R, A] = Kleisli[F, R, A](r => fa.run(f(r)))
}

//
// (* *) -> *
//

private[scalaz] trait KleisliArrIdArr[F[_]] extends ArrId[({type λ[α, β] = Kleisli[F, α, β]})#λ] {
  implicit def F: Pointed[F]

  def id[A]: Kleisli[F, A, A] = kleisli(a => F.point(a))

  def arr[A, B](f: (A) => B): Kleisli[F, A, B] = kleisli(a => F.point(f(a)))
}

private[scalaz] trait KleisliCategory[F[_]] extends Category[({type λ[α, β] = Kleisli[F, α, β]})#λ] with KleisliArrIdArr[F] {
  implicit def F: Monad[F]

  def compose[A, B, C](bc: Kleisli[F, B, C], ab: Kleisli[F, A, B]): Kleisli[F, A, C] = ab >=> bc
}

private[scalaz] trait KleisliArrow[F[_]] extends Arrow[({type λ[α, β] = Kleisli[F, α, β]})#λ] with KleisliCategory[F] {
  implicit def F: Monad[F]

  def first[A, B, C](f: Kleisli[F, A, B]): Kleisli[F, (A, C), (B, C)] = kleisli[F, (A, C), (B, C)] {
    case (a, c) => F.map(f.run(a))((b: B) => (b, c))
  }
}
