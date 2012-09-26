package scalaz

/**
 * Represents a function `A => F[B]` where `[F: Applicative]`.
 */
trait AppFunc[F[_], A, B] { self =>
  def runA(a: A): F[B]
  implicit def F: Applicative[F]

  import AppFunc._

  /** alias for andThenA */
  def @>>>[G[_], C](g: AppFunc[G, B, C]) = andThenA(g)
  /** compose `A => F[B]` and `B => G[C]` into `A => F[G[C]]` */ 
  def andThenA[G[_], C](g: AppFunc[G, B, C]) = g.composeA(self)

  /** alias for composeA */
  def <<<@[G[_], C](g: AppFunc[G, C, A]) = composeA(g)
  /** compose `A => F[B]` and `C => G[A]` into `C => G[F[B]]` */ 
  def composeA[G[_], C](g: AppFunc[G, C, A]): AppFunc[({type λ[α] = G[F[α]]})#λ, C, B] = new AppFunc[({type λ[α] = G[F[α]]})#λ, C, B] {
    def runA(c: C): G[F[B]] = g.F.map(g.runA(c): G[A]) { a: A => self.runA(a) }
    def F = g.F compose self.F
  }

  /** alias for productA  */
  def @&&&[G[_]](g: AppFunc[G, A, B]) = productA(g)
  def &&&@[G[_]](g: AppFunc[G, A, B]) = g.productA(self)
  /** compose `A => F[B]` and `A => G[B]` into `A => (F[B], G[B])` */
  def productA[G[_]](g: AppFunc[G, A, B]): AppFunc[({type λ[α] = (F[α], G[α])})#λ, A, B] = new AppFunc[({type λ[α] = (F[α], G[α])})#λ, A, B] {
    def runA(a: A): (F[B], G[B]) = (self.runA(a), g.runA(a))
    def F = self.F product g.F
  }

  def mapA[C](f: B => C): AppFunc[F, A, C] =
    appfunc(a => F.map(self.runA(a))(f))

  def traverse[G[_]](value: G[A])(implicit G: Traverse[G]): F[G[B]] =
    G.traverse(value)(a => self.runA(a))(F)
}

//
// Prioritized Implicits for type class instances
//
trait AppFuncInstances3 {
  implicit def AppFuncFunctor[F[_], R]: Functor[({type λ[α] = AppFunc[F, R, α]})#λ] = new AppFuncFunctor[F, R] {}
}

trait AppFuncInstances2 extends AppFuncInstances3 {
  implicit def AppFuncPointed[F[_], R](implicit F0: Applicative[F]): Pointed[({type λ[α] = AppFunc[F, R, α]})#λ] = new AppFuncPointed[F, R] {
    implicit def F: Applicative[F] = F0
  }
  implicit def AppFuncApply[F[_], R](implicit F0: Applicative[F]): Apply[({type λ[α] = AppFunc[F, R, α]})#λ] = new AppFuncApply[F, R] {
    implicit def F: Applicative[F] = F0
  }
}

trait AppFuncInstances1 extends AppFuncInstances2 {
  implicit def AppFuncApplicative[F[_], R](implicit F0: Applicative[F]): Applicative[({type λ[α] = AppFunc[F, R, α]})#λ] = new AppFuncApplicative[F, R] {
    implicit def F: Applicative[F] = F0
  }
}

trait AppFuncInstances extends AppFuncInstances1 {
}

trait AppFuncFunctions {
  def appfunc[M[_], A, B](f: A => M[B])(implicit F0: Applicative[M]): AppFunc[M, A, B] = new AppFunc[M, A, B] {
    def F = F0
    def runA(a: A) = f(a)
  }
  def appfuncU[A, R](f: A => R)(implicit F0: Unapply[Applicative, R]): AppFunc[F0.M, A, F0.A] = new AppFunc[F0.M, A, F0.A] {
    def F = F0.TC
    def runA(a: A) = F0(f(a))
  }
  def monoidal[A, R](f: A => R)(implicit F0: Monoid[R]) = appfunc[({type λ[α] = R})#λ, A, R](f)(F0.applicative)
}

object AppFuncU {
  def apply[A, R](f: A => R)(implicit F0: Unapply[Applicative, R]): AppFunc[F0.M, A, F0.A] = AppFunc.appfuncU(f)
}

object AppFunc extends AppFuncFunctions with AppFuncInstances {
  def apply[M[_], A, B](f: A => M[B])(implicit F0: Applicative[M]): AppFunc[M, A, B] = appfunc(f)
}

//
// Implementation traits for type class instances
//

import AppFunc.appfunc

private[scalaz] trait AppFuncFunctor[F[_], R] extends Functor[({type λ[α] = AppFunc[F, R, α]})#λ] {
  override def map[A, B](fa: AppFunc[F, R, A])(f: A => B): AppFunc[F, R, B] = fa mapA f
}

private[scalaz] trait AppFuncPointed[F[_], R] extends Pointed[({type λ[α] = AppFunc[F, R, α]})#λ] with AppFuncFunctor[F, R] {
  implicit def F: Applicative[F]
  def point[A](a: => A): AppFunc[F, R, A] = appfunc((r: R) => F.point(a))
}

private[scalaz] trait AppFuncApply[F[_], R] extends Apply[({type λ[α] = AppFunc[F, R, α]})#λ] with AppFuncFunctor[F, R] {
  implicit def F: Applicative[F]
  def ap[A, B](fa: => AppFunc[F, R, A])(f: => AppFunc[F, R, (A) => B]): AppFunc[F, R, B] = appfunc(r => F.ap(fa.runA(r))(f.runA(r)))
}

private[scalaz] trait AppFuncApplicative[F[_], R] extends Applicative[({type λ[α] = AppFunc[F, R, α]})#λ] with AppFuncApply[F, R] with AppFuncPointed[F, R] {
  implicit def F: Applicative[F]
}
