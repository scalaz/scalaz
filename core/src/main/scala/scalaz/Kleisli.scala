package scalaz

sealed trait Kleisli[M[_], A, B] {
  def apply(a: A): M[B]

  import Kleisli._

  // TODO provide non-symbolic aliases.

  def >=>[C](k: Kleisli[M, B, C])(implicit b: Bind[M]): Kleisli[M, A, C] = kleisli((a: A) => b.bind(this(a))(k(_)))

  def >=>[C](k: B => M[C])(implicit b: Bind[M]): Kleisli[M, A, C] = >=>(kleisli(k))

  def <=<[C](k: Kleisli[M, C, A])(implicit b: Bind[M]): Kleisli[M, C, B] = k >=> this

  def <=<[C](k: C => M[A])(implicit b: Bind[M]): Kleisli[M, C, B] = kleisli(k) >=> this

  def compose[N[_]](f: M[B] => N[B]): Kleisli[N, A, B] = kleisli((a: A) => f(this(a)))

  def traverse[F[_], AA <: A](f: F[AA])(implicit M: Applicative[M], F: Traverse[F]): M[F[B]] =
    F.traverse(f)(Kleisli.this(_))

  def =<<[AA <: A](a: M[AA])(implicit m: Bind[M]): M[B] = m.bind(a)(apply _)

  def map[C](f: B => C)(implicit m: Functor[M]): Kleisli[M, A, C] =
    kleisli(a => m.map(apply(a))(f))

  def flatMap[C](f: B => M[C])(implicit M: Bind[M]): Kleisli[M, A, C] =
    kleisli(a => M.bind(apply(a))(f))
  
  def flatMapK[C](f: B => Kleisli[M, A, C])(implicit M: Bind[M]): Kleisli[M, A, C] =
    kleisli((r: A) => M.bind[B,C](apply(r))(((b: B) => f(b).apply(r))))
}

//
// Prioritized Implicits for type class instances
//

trait KleislisLow1 {
  implicit def kleisliFunctor[F[_], R](implicit F0: Functor[F]): Functor[({type λ[α] = Kleisli[F, R, α]})#λ] = new KleisliFunctor[F, R] {
    implicit def F: Functor[F] = F0
  }
}

trait KleislisLow0 extends KleislisLow1 {
  implicit def kleisliPointed[F[_], R](implicit F0: Pointed[F]): Pointed[({type λ[α] = Kleisli[F, R, α]})#λ] = new KleisliPointed[F, R] {
    implicit def F: Pointed[F] = F0
  }
}

trait Kleislis extends KleislisLow0 {
  /** Construct a Kliesli from a Function1 */
  def kleisli[M[_], A, B](f: A => M[B]): Kleisli[M, A, B] = new Kleisli[M, A, B] {
    def apply(a: A) = f(a)
  }

  /** Implicitly unwrap the Function1 represented by the Kleisli */
  implicit def kleisliFn[M[_], A, B](k: Kleisli[M, A, B]): A => M[B] = (a: A) => k(a)

  /** Pure Kleisli arrow */
  def ask[M[_] : Monad, A]: Kleisli[M, A, A] = kleisli(a => implicitly[Monad[M]].pure(a))

  implicit def kleisliArr[F[_]](implicit F0: Pointed[F]): Arr[({type λ[α, β]=Kleisli[F, α, β]})#λ] = new Arr[({type λ[α, β]=Kleisli[F, α, β]})#λ] {
    def arr[A, B](f: (A) => B): Kleisli[F, A, B] = kleisli(a => F0.pure(f(a)))
  }

  implicit def kleisliCategory[F[_]](implicit F: Monad[F]) = new Category[({type λ[α, β] = Kleisli[F, α, β]})#λ] {
    def id[A]: Kleisli[F, A, A] = kleisli(a => F.pure(a))

    def compose[A, B, C](f: Kleisli[F, B, C], g: Kleisli[F, A, B]): Kleisli[F, A, C] = g >=> f
  }

  implicit def kleisliMonad[F[_], R](implicit F0: Monad[F]): Monad[({type λ[α] = Kleisli[F, R, α]})#λ] = new KleisliMonad[F, R] {
    implicit def F: Monad[F] = F0
  }
}

object Kleisli extends Kleislis

//
// Implementation traits for type class instances
//

private[scalaz] trait KleisliFunctor[F[_], R] extends Functor[({type λ[α] = Kleisli[F, R, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: Kleisli[F, R, A])(f: A => B): Kleisli[F, R, B] = fa map f
}

private[scalaz] trait KleisliPointed[F[_], R] extends Pointed[({type λ[α] = Kleisli[F, R, α]})#λ] with KleisliFunctor[F, R] {
  implicit def F: Pointed[F]

  def pure[A](a: => A): Kleisli[F, R, A] = Kleisli.kleisli((r: R) => F.pure(a))
}

private[scalaz] trait KleisliMonad[F[_], R] extends Monad[({type λ[α] = Kleisli[F, R, α]})#λ] with KleisliPointed[F, R] {
  implicit def F: Monad[F]

  def bind[A, B](fa: Kleisli[F, R, A])(f: A => Kleisli[F, R, B]): Kleisli[F, R, B] = fa flatMapK f
}

