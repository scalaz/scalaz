package scalaz

trait NaturalTransformation[-F[_], +G[_]] {
  self =>
  def apply[A](fa: F[A]): G[A]

  def compose[E[_]](f: E ~> F): E ~> G = new (E ~> G) {
    def apply[A](ea: E[A]) = self(f(ea))
  }
}

trait NaturalTransformations {
  /**A function type encoded as a natural transformation */
  type ->[A, B] = (({type λ[α]=A})#λ) ~> (({type λ[α]=B})#λ)

  /**A universally quantified identity function */
  def id = new (Id ~> Id) {
    def apply[A](a: A) = a
  }

  implicit def natToFunction[F[_], G[_], A](f: F ~> G): F[A] => G[A] = x => f(x)
}

object NaturalTransformation extends NaturalTransformations

trait BiNaturalTransformation[-F[_, _], +G[_, _]] {
  self =>
  def apply[A, B](f: F[A, B]): G[A, B]

  def compose[E[_, _]](f: BiNaturalTransformation[E, F]) = new BiNaturalTransformation[E, G] {
    def apply[A, B](eab: E[A, B]): G[A, B] = self(f(eab))
  }
}

/** A constrained natural transformation */
trait ConstrainedNaturalTransformation[F[_], G[_], E[_]] {
  def apply[A: E](f: F[A]): G[A]
}

/** A constrained transformation natural in both sides of a bifunctor */
trait BiConstrainedNaturalTransformation[F[_,_], G[_,_], C[_], E[_]] {
  def apply[A: C, B: E](f: F[A,B]): G[A,B]
}

trait DiNaturalTransformation[F[_,_], G[_,_]] {
  def apply[A](f: F[A,A]): G[A,A]
}

// TODO needed, or just use type lambdas?
//type Thunk[A] = () => A
//
//trait Konst[A] {
//  type Apply[B] = A
//}
//
//trait Biff[P[_,_], F[_], G[_]] {
//  type Apply[A, B] = P[F[A], G[B]]
//}
//
//trait On[P[_,_], F[_]] {
//  type Apply[A, B] = P[F[A], F[B]]
//}
//
//trait Distributes[F[_], G[_]] {
//  def apply[A](f: F[G[A]]): G[F[A]]
//}
