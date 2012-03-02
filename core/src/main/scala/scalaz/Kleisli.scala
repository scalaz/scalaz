package scalaz

/**
 * Represents a function `A => M[B]`.
 */
sealed trait Kleisli[M[_], A, B] { self =>
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

  def map[C](f: B => C)(implicit M: Functor[M]): Kleisli[M, A, C] =
    kleisli(a => M.map(run(a))(f))
    
  def mapK[N[_], C](f: M[B] => N[C]): Kleisli[N, A, C] = 
    kleisli(a => f(run(a)))

  def flatMapK[C](f: B => M[C])(implicit M: Bind[M]): Kleisli[M, A, C] =
    kleisli(a => M.bind(run(a))(f))

  def flatMap[C](f: B => Kleisli[M, A, C])(implicit M: Bind[M]): Kleisli[M, A, C] =
    kleisli((r: A) => M.bind[B, C](run(r))(((b: B) => f(b).run(r))))

  def rwst[W, S](implicit M: Functor[M], W: Monoid[W]): ReaderWriterStateT[M, A, W, S, B] = ReaderWriterStateT(
    (r, s) => M.map(self(r)) {
      b => (W.zero, b, s)
    }
  )
}

//
// Prioritized Implicits for type class instances
//
trait KleisliInstances4 {
  implicit def kleisliFunctor[F[_], R](implicit F0: Functor[F]): Functor[({type λ[α] = Kleisli[F, R, α]})#λ] = new KleisliFunctor[F, R] {
    implicit def F: Functor[F] = F0
  }

  implicit def kleisliIdFunctor[R]: Functor[({type λ[α] = Kleisli[Id, R, α]})#λ] = kleisliFunctor[Id, R]
}

trait KleisliInstances3 extends KleisliInstances4 {
  implicit def kleisliPointed[F[_], R](implicit F0: Pointed[F]): Pointed[({type λ[α] = Kleisli[F, R, α]})#λ] = new KleisliPointed[F, R] {
    implicit def F: Pointed[F] = F0
  }
  implicit def kleisliIdPointed[R]: Pointed[({type λ[α] = Kleisli[Id, R, α]})#λ] = kleisliPointed[Id, R]

  implicit def kleisliApply[F[_], R](implicit F0: Apply[F]): Apply[({type λ[α] = Kleisli[F, R, α]})#λ] = new KleisliApply[F, R] {
    implicit def F: Apply[F] = F0
  }
  implicit def kleisliIdApply[R]: Apply[({type λ[α] = Kleisli[Id, R, α]})#λ] = kleisliApply[Id, R]
}

trait KleisliInstances2 extends KleisliInstances3 {
  implicit def kleisliApplicative[F[_], R](implicit F0: Applicative[F]): Applicative[({type λ[α] = Kleisli[F, R, α]})#λ] = new KleisliApplicative[F, R] {
    implicit def F: Applicative[F] = F0
  }
  implicit def kleisliIdApplicative[R]: Applicative[({type λ[α] = Kleisli[Id, R, α]})#λ] = kleisliApplicative[Id, R]
  implicit def kleislPlus[F[_], A](implicit F0: Plus[F]) = new KleisliPlus[F, A] {
    implicit def F = F0
  }
}

trait KleisliInstances1 extends KleisliInstances2 {
  implicit def kleisliApplicativePlus[F[_], R](implicit F0: ApplicativePlus[F]): ApplicativePlus[({type λ[α] = Kleisli[F, R, α]})#λ] = new ApplicativePlus[({type λ[α] = Kleisli[F, R, α]})#λ] with KleisliApplicative[F, R] with KleisliPlusEmpty[F, R] {
    implicit def F: ApplicativePlus[F] = F0
  }
  implicit def kleisliArrId[F[_]](implicit F0: Pointed[F]) = new KleisliArrIdArr[F] {
    implicit def F: Pointed[F] = F0
  }
  implicit def kleisliChoice[F[_]] = new KleisliChoice[F] {}
  implicit def kleisliSemigroup[F[_], A, B](implicit FB0: Semigroup[F[B]]) = new KleisliSemigroup[F, A, B] {
    implicit def FB = FB0
  }
}

trait KleisliInstances0 extends KleisliInstances1 {
  implicit def kleisliMonadPlus[F[_], A](implicit F0: MonadPlus[F]) = new KleisliMonadPlus[F, A] {
    implicit def F = F0
  }
}

trait KleisliInstances extends KleisliInstances0 {
  implicit def kleisliArrow[F[_]](implicit F0: Monad[F]) = new KleisliArrow[F] {
    implicit def F: Monad[F] = F0
  }

  implicit def kleisliMonadReader[F[_], R](implicit F0: Monad[F]) = new KleisliMonadReader[F, R] {
    implicit def F: Monad[F] = F0
  }

  implicit def kleisliIdMonadReader[R] = kleisliMonadReader[Id, R]

  implicit def kleisliMonoid[F[_], A, B](implicit FB0: Monoid[F[B]]) = new KleisliMonoid[F, A, B] {
    implicit def FB = FB0
  }
  implicit def kleisliPlusEmpty[F[_], A](implicit F0: PlusEmpty[F]) = new KleisliPlusEmpty[F, A] {
    implicit def F = F0
  }
  implicit def kleisliMonadTrans[R]: Hoist[({type λ[α[_], β] = Kleisli[α, R, β]})#λ] = new KleisliHoist[R] {}
}

trait KleisliFunctions {
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

private[scalaz] trait KleisliMonad[F[_], R] extends Monad[({type λ[α] = Kleisli[F, R, α]})#λ] with KleisliApplicative[F, R] {
  implicit def F: Monad[F]
  def bind[A, B](fa: Kleisli[F, R, A])(f: A => Kleisli[F, R, B]): Kleisli[F, R, B] = fa flatMap f
}

private[scalaz] trait KleisliMonadReader[F[_], R] extends MonadReader[({type f[s, a] = Kleisli[F, s, a]})#f, R] with KleisliApplicative[F, R] with KleisliMonad[F, R] {
  implicit def F: Monad[F]

  def ask: Kleisli[F, R, R] = Kleisli[F, R, R](r => F.point(r))
  def local[A](f: (R) => R)(fa: Kleisli[F, R, A]): Kleisli[F, R, A] = Kleisli[F, R, A](r => fa.run(f(r)))
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

//
// (* *) -> *
//

private[scalaz] trait KleisliArrIdArr[F[_]] extends ArrId[({type λ[α, β] = Kleisli[F, α, β]})#λ] {
  implicit def F: Pointed[F]

  def id[A]: Kleisli[F, A, A] = kleisli(a => F.point(a))

  def arr[A, B](f: (A) => B): Kleisli[F, A, B] = kleisli(a => F.point(f(a)))
}

private[scalaz] trait KleisliChoice[F[_]] extends Choice[({type λ[α, β] = Kleisli[F, α, β]})#λ] {
  def choice[A, B, C](f: => Kleisli[F, A, C], g: => Kleisli[F, B, C]): Kleisli[F, Either[A, B], C] =
    Kleisli {
      case Left(a) => f run a
      case Right(b) => g run b
    }
}

private[scalaz] trait KleisliSplit[F[_]] extends Split[({type λ[α, β] = Kleisli[F, α, β]})#λ] {
  implicit def F: Monad[F]

  def split[A, B, C, D](f: Kleisli[F, A, B], g: Kleisli[F, C, D]): Kleisli[F, (A,  C), (B, D)] =
    Kleisli {
      case (a, c) =>
        F.bind(f run a)(b => F.map(g run c)(d => (b, d)))
    }
}

private[scalaz] trait KleisliArrow[F[_]] extends Arrow[({type λ[α, β] = Kleisli[F, α, β]})#λ] with KleisliArrIdArr[F] {
  implicit def F: Monad[F]

  def compose[A, B, C](bc: Kleisli[F, B, C], ab: Kleisli[F, A, B]): Kleisli[F, A, C] = ab >=> bc

  def first[A, B, C](f: Kleisli[F, A, B]): Kleisli[F, (A, C), (B, C)] = kleisli[F, (A, C), (B, C)] {
    case (a, c) => F.map(f.run(a))((b: B) => (b, c))
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
