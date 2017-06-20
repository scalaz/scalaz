package scalaz
package data

import Prelude._

/**
  * The existence of a value of type `LeibnizK[A[_], B[_]]` implies that A ≡ B.
  * For an explanation see [[Is]].
  *
  * @see [[=~=]] `A =~= B` is a type synonym to `LeibnizK[A, B]`
  */
sealed abstract class IsK[A[_], B[_]] private[IsK]() { ab =>
  import IsK._

  /**
    * To create an instance of `LeibnizK[A[_], B[_]]` you must show that
    * for every choice of `F[_[_]]` you can convert `F[A]` to `F[B]`.
    */
  def subst[F[_[_]]](fa: F[A]): F[B]

//  def substK[F[_[_], _]]: F[A, ?] ~> F[B, ?] = {
//    type f[α[_]] = F[A, ?] ~> F[α, ?]
//    subst[f](FunctionK.id[F[A, ?]])
//  }

  /**
    * Substitution on identity brings about a direct coercion function.
    */
//  final def coerce: A ~> B = {
//    type f[α[_]] = A ~> α
//    subst[f](FunctionK.id[A])
//  }

  /**
    * Equality is transitive relation and its witnesses can be composed
    * in a chain much like functions.
    *
    * @see [[compose]]
    */
  final def andThen[C[_]](bc: B =~= C): A =~= C = {
    type f[α[_]] = A =~= α
    bc.subst[f](ab)
  }

  /**
    * Equality is transitive relation and its witnesses can be composed
    * in a chain much like functions.
    *
    * @see [[andThen]]
    */
  final def compose[Z[_]](za: Z =~= A): Z =~= B =
    za.andThen(ab)

  /**
    * Equality is symmetric relation and therefore can be flipped around.
    * Flipping is its own inverse, so `x.flip.flip == x`.
    */
  final def flip: B =~= A = {
    type f[α[_]] = α =~= A
    ab.subst[f](refl)
  }

  /**
    * Given `A =~= B` we can prove that `F[A] === F[B]`.
    *
    * @see [[IsK.lower]]
    * @see [[IsK.lower2]]
    */
  final def lower[F[_[_]]]: F[A] === F[B] =
    IsK.lower(ab)

  /**
    * Given `A =~= B` and `I =~= J` we can prove that `F[A, I] === F[B, J]`.
    *
    * @see [[IsK.lower]]
    * @see [[IsK.lower2]]
    * @see [[IsK.lower3]]
    */
  final def lower2[F[_[_], _[_]]]: PartiallyAppliedLower2[F] =
    new PartiallyAppliedLower2[F]
  final class PartiallyAppliedLower2[F[_[_], _[_]]] {
    def apply[I[_], J[_]](ij: I =~= J): F[A, I] === F[B, J] =
      IsK.lower2(ab, ij)
  }

  /**
    * Given `A =~= B` we can prove that `F[A, ?] =~= F[B, ?]`.
    *
    * @see [[IsK.lift]]
    * @see [[IsK.lift2]]
    */
  final def lift[F[_[_], _]]: F[A, ?] =~= F[B, ?] =
    IsK.lift(ab)

  /**
    * Given `A =~= B` and `I =~= J` we can prove that
    * `F[A, I, ?] =~= F[B, J, ?]`.
    *
    * @see [[IsK.lift]]
    * @see [[IsK.lift2]]
    * @see [[IsK.lift3]]
    */
  final def lift2[F[_[_], _[_], _]]: PartiallyAppliedLift2[F] =
    new PartiallyAppliedLift2[F]
  final class PartiallyAppliedLift2[F[_[_], _[_], _]] {
    def apply[I[_], J[_]](ij: I =~= J): F[A, I, ?] =~= F[B, J, ?] =
      IsK.lift2(ab, ij)
  }

  /**
    * Given `A =~= B` we can convert `X ~> A` into `X ~> B`.
    */
//  final def onF[X[_]](fa: X ~> A): X ~> B = {
//    type f[α[_]] = X ~> α
//    subst[f](fa)
//  }
}

object IsK {
  def apply[A[_], B[_]](implicit ab: IsK[A, B]): IsK[A, B] = ab

  final case class Refl[A[_]]() extends IsK[A, A] {
    def subst[F[_[_]]](fa: F[A]): F[A] = fa
  }
  private[this] val anyRefl: Any =~= Any = Refl[Any]()

  /**
    * Unsafe coercion between types. `unsafeForce` abuses `asInstanceOf` to
    * explicitly coerce types. It is unsafe, but needed where Leibnizian
    * equality isn't sufficient.
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def unsafeForce[A[_], B[_]]: A =~= B =
  anyRefl.asInstanceOf[A =~= B]

  /**
    * Equality is reflexive relation.
    */
  implicit def refl[A[_]]: A =~= A = unsafeForce[A, A]

  /**
    * Given `A =~= B` we can prove that `F[A] === F[B]`.
    *
    * @see [[lift2]]
    * @see [[lift3]]
    */
  def lower[F[_[_]], A[_], B[_]]
  (ab: A =~= B): F[A] === F[B] = {
    type f[α[_]] = F[A] === F[α]
    ab.subst[f](Is.refl)
  }

  /**
    * Given `A =~= B` and `I =~= J` we can prove that
    * `F[A, I] === F[B, J]`.
    *
    * @see [[lift]]
    * @see [[lift3]]
    */
  def lower2[F[_[_], _[_]], A[_], B[_], I[_], J[_]]
  (ab: A =~= B, ij: I =~= J): F[A, I] === F[B, J] = {
    type f1[α[_]] = F[A, I] === F[α, I]
    type f2[α[_]] = F[A, I] === F[B, α]
    ij.subst[f2](ab.subst[f1](Is.refl))
  }

  /**
    * Given `A =~= B`, `I =~= J`, and `M =~= N` we can prove that
    * `F[A, I] === F[B, J]`.
    *
    * @see [[lift]]
    * @see [[lift2]]
    */
  def lower3[F[_[_], _[_], _[_]], A[_], B[_], I[_], J[_], M[_], N[_]]
  (ab: A =~= B, ij: I =~= J, mn: M =~= N): F[A, I, M] === F[B, J, N] = {
    type f1[α[_]] = F[A, I, M] === F[α, I, M]
    type f2[α[_]] = F[A, I, M] === F[B, α, M]
    type f3[α[_]] = F[A, I, M] === F[B, J, α]
    mn.subst[f3](ij.subst[f2](ab.subst[f1](Is.refl)))
  }

  /**
    * Given `A =~= B` we can prove that `F[A, ?] =~= F[B, ?]`.
    *
    * @see [[lift2]]
    * @see [[lift3]]
    */
  def lift[F[_[_], ?], A[_], B[_]]
  (ab: A =~= B): F[A, ?] =~= F[B, ?] = {
    type f[α[_]] = F[A, ?] =~= F[α, ?]
    ab.subst[f](refl[F[A, ?]])
  }

  /**
    * Given `A =~= B` and `I =~= J` we can prove that
    * `F[A, I, ?] =~= F[B, J, ?]`.
    *
    * @see [[lift]]
    * @see [[lift3]]
    */
  def lift2[F[_[_], _[_], _], A[_], B[_], I[_], J[_]]
  (ab: A =~= B, ij: I =~= J): F[A, I, ?] =~= F[B, J, ?] = {
    type f1[α[_]] = F[A, I, ?] =~= F[α, I, ?]
    type f2[α[_]] = F[A, I, ?] =~= F[B, α, ?]
    ij.subst[f2](ab.subst[f1](refl[F[A, I, ?]]))
  }

  /**
    * Given `A =~= B`, `I =~= J`, and `M =~= N` we can prove that
    * `F[A, I, ?] =~= F[B, J, ?]`.
    *
    * @see [[lift]]
    * @see [[lift2]]
    */
  def lift3[F[_[_], _[_], _[_], _], A[_], B[_], I[_], J[_], M[_], N[_]]
  (ab: A =~= B, ij: I =~= J, mn: M =~= N): F[A, I, M, ?] =~= F[B, J, N, ?] = {
    type f1[α[_]] = F[A, I, M, ?] =~= F[α, I, M, ?]
    type f2[α[_]] = F[A, I, M, ?] =~= F[B, α, M, ?]
    type f3[α[_]] = F[A, I, M, ?] =~= F[B, J, α, ?]
    mn.subst[f3](ij.subst[f2](ab.subst[f1](refl[F[A, I, M, ?]])))
  }
}