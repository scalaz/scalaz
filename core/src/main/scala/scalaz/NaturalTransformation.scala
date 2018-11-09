package scalaz

import Id._

/** A universally quantified function, usually written as `F ~> G`,
  * for symmetry with `A => B`.
  *
  * Can be used to encode first-class functor transformations in the
  * same way functions encode first-class concrete value morphisms;
  * for example, `sequence` from [[scalaz.Traverse]] and `cosequence`
  * from [[scalaz.Distributive]] give rise to `([a]T[A[a]]) ~>
  * ([a]A[T[a]])`, for varying `A` and `T` constraints.
  */
trait NaturalTransformation[-F[_], +G[_]] {
  self =>
  def apply[A](fa: F[A]): G[A]

  def compose[E[_]](f: E ~> F): E ~> G = λ[E ~> G](
    ea => self(f(ea))
  )

  def andThen[H[_]](f: G ~> H): F ~> H =
    f compose self

  /**
    * Combines this [[scalaz.NaturalTransformation]] with another one to create one
    * that can transform [[scalaz.Coproduct]].
    *
    * The current NaturalTransformation will be used to transform the Left (`F`) value of
    * the [[scalaz.Coproduct]] while the other one will be used to transform the Right (`H`) value.
    */
  def or[H[_], F0[A] <: F[A], G0[A] >: G[A]](hg: H ~> G0): Coproduct[F0, H, ?] ~> G0 =
    λ[Coproduct[F0, H, ?] ~> G0](_.fold(self, hg))
}

trait NaturalTransformations {
  /** A function type encoded as a natural transformation by adding a
    * phantom parameter.
    */
  type ->[A, B] = λ[α => A] ~> λ[α => B]

  /** `refl` specialized to [[scalaz.Id.Id]]. */
  def id: Id ~> Id =
    λ[Id ~> Id](a => a)

  /** A universally quantified identity function */
  def refl[F[_]]: F ~> F =
    λ[F ~> F](fa => fa)
}

object NaturalTransformation extends NaturalTransformations {
  /**
   * Construct a natural transformation over a coproduct from its parts.
   * Useful for combining Free interpreters.
   */
  def or[F[_], G[_], H[_]](fg: F ~> G, hg: H ~> G): Coproduct[F, H, ?] ~> G =
    λ[Coproduct[F, H, ?] ~> G](_.fold(fg, hg))

  /**
   * Like Hoist, for Functors, when we already know how to transform `F ~> G`.
   */
  def liftMap[F[_], G[_], H[_]: Functor](in: F ~> G): λ[α => H[F[α]]] ~> λ[α => H[G[α]]] =
    λ[λ[α => H[F[α]]] ~> λ[α => H[G[α]]]](fa => Functor[H].map(fa)(in.apply))
}

/** A function universally quantified over two parameters. */
trait BiNaturalTransformation[-F[_, _], +G[_, _]] {
  self =>
  def apply[A, B](f: F[A, B]): G[A, B]

  def compose[E[_, _]](f: BiNaturalTransformation[E, F]) =
    new BiNaturalTransformation[E, G] {
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
trait Konst[A] {
  type Apply[B] = A
}
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
