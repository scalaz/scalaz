package scalaz
package types

import scala.{ Any, AnyVal }

/**
 * Liskov substitutability: A better `<:<`.
 *
 * `Liskov[A, B]` witnesses that `A` can be used in any negative context
 * that expects a `B`. (e.g. if you could pass an `A` into any function
 * that expects a `B`.)
 *
 * @see [[As]] an unbounded version of [[Liskov]].
 */
sealed abstract class Liskov[-L, +H >: L, -A >: L <: H, +B >: L <: H] { ab =>

  /**
   * Substitution into a covariant context.
   *
   * @see [[substCt]]
   */
  def substCv[F[+ _ >: L <: H]](x: F[A]): F[B]

  /**
   * Substitution into a contravariant context.
   *
   * @see [[substCv]]
   */
  def substCt[F[- _ >: L <: H]](x: F[B]): F[A] = {
    type f[+a >: L <: H] = F[a] => F[A]
    substCv[f](identity[F[A]])(x)
  }

  /**
   * Substitution on identity brings about a direct coercion function
   * of the same form that [[<:<]] provides.
   *
   * @see [[coerce]]
   */
  final def apply(a: A): B =
    coerce(a)

  /**
   * Subtyping is transitive and its witnesses can be composed in a
   * chain much like functions.
   */
  final def andThen[L2 <: L, H2 >: H, C >: L2 <: H2](bc: Liskov[L2, H2, B, C]): Liskov[L2, H2, A, C] =
    Liskov.compose[L2, H2, A, B, C](bc, ab)

  /**
   * Subtyping is transitive and its witnesses can be composed in a
   * chain much like functions.
   *
   * @see [[andThen]]
   */
  final def compose[L2 <: L, H2 >: H, Z >: L2 <: H2](za: Liskov[L2, H2, Z, A]): Liskov[L2, H2, Z, B] =
    za.andThen(ab)

  /**
   * Substitution on identity brings about a direct coercion function
   * of the same form that [[<:<]] provides.
   *
   * @see [[apply]]
   */
  final def coerce(a: A): B = {
    type f[+a] = a
    substCv[f](a)
  }

  /**
   * Given `Liskov[L, H, A, B]` we can convert `(X => A)` into `(X => B)`.
   */
  final def onCvF[X](fa: X => A): X => B = {
    type f[+a] = X => a
    substCv[f](fa)
  }

  /**
   * Given `Liskov[L, H, A, B]` we can convert `(B => X)` into `(A => X)`.
   */
  def onCtF[X](fa: B => X): A => X = {
    type f[-a] = a => X
    substCt[f](fa)
  }

  /**
   * Given `Liskov[L, H, A, B]`, prove `A <:< B`.
   */
  final def toPredef: A <:< B = {
    type f[+a] = A <:< a
    substCv[f](implicitly[A <:< A])
  }

  /**
   * Given `Liskov[L, H, A, B]`, prove `A <~< B`.
   */
  final def toAs: A <~< B = {
    type f[+a] = A <~< a
    substCv[f](implicitly[A <~< A])
  }
}

object Liskov {

  def apply[L, H >: L, A >: L <: H, B >: L <: H](implicit ab: Liskov[L, H, A, B]): Liskov[L, H, A, B] = ab

  private[this] final case class Refl[A]() extends Liskov[A, A, A, A] {
    def substCv[F[+ _ >: A <: A]](x: F[A]): F[A] = x
  }

  private[this] val reflAll = ∀.of[λ[α => Liskov[α, α, α, α]]].from(new Refl)

  /**
   * Subtyping relation is reflexive.
   */
  implicit def refl[A]: Liskov[A, A, A, A] = reflAll[A]

  /**
   * Reify Scala's subtyping relationship into an evidence value.
   */
  implicit def reify[
    L,
    H >: L,
    A >: L <: (H with B),
    B >: L <: H
  ]: Liskov[L, H, A, B] = refl[A]

  /**
   * Subtyping is transitive relation and its witnesses can be composed
   * in a chain much like functions.
   *
   * @see [[Liskov.compose]]
   * @see [[Liskov.andThen]]
   */
  def compose[L, H >: L, A >: L <: H, B >: L <: H, C >: L <: H](bc: Liskov[L, H, B, C],
                                                                ab: Liskov[L, H, A, B]): Liskov[L, H, A, C] =
    bc.substCv[λ[`+α >: L <: H` => Liskov[L, H, A, α]]](ab)

  implicit class LiskovOps[L, H >: L, A >: L <: H, B >: L <: H](val ab: Liskov[L, H, A, B]) extends AnyVal {
    def liftCvF[LF, HF >: LF, F[_] >: LF <: HF](implicit F: IsCovariant[F]): Liskov[LF, HF, F[A], F[B]] =
      fromAs[LF, HF, F[A], F[B]](F.liftLiskov(ab.toAs))

    def liftCtF[LF, HF >: LF, F[_] >: LF <: HF](implicit F: IsContravariant[F]): Liskov[LF, HF, F[B], F[A]] =
      fromAs[LF, HF, F[B], F[A]](F.liftLiskov(ab.toAs))

    def substCoF[LF, HF >: LF, F[_] >: LF <: HF](fa: F[A])(implicit F: IsCovariant[F]): F[B] =
      liftCvF[LF, HF, F].coerce(fa)

    def substCt[LF, HF >: LF, F[_] >: LF <: HF](fb: F[B])(implicit F: IsContravariant[F]): F[A] =
      liftCtF[LF, HF, F].coerce(fb)
  }

  /**
   * Subtyping is antisymmetric.
   */
  def bracket[L, H >: L, A >: L <: H, B >: L <: H](f: Liskov[L, H, A, B],
                                                   g: Liskov[L, H, B, A]): Leibniz[L, H, A, B] = {
    val (_, _) = (f, g)
    Leibniz.unsafeForce[L, H, A, B]
  }

  /**
   * Given `A <:< B` with `A >: L <: H` and `B >: L <: H`,
   * prove `Liskov[L, H, A, B]`.
   */
  def fromPredef[L, H >: L, A >: L <: H, B >: L <: H](ev: A <:< B): Liskov[L, H, A, B] =
    fromAs(As.fromPredef(ev))

  /**
   * Given `A <~< B` with `A >: L <: H` and `B >: L <: H`,
   * prove `Liskov[L, H, A, B]`.
   */
  def fromAs[L, H >: L, A >: L <: H, B >: L <: H](ev: A <~< B): Liskov[L, H, A, B] = {
    val _ = ev
    unsafeForce[L, H, A, B]
  }

  /**
   * Unsafe coercion between types. `unsafeForce` abuses `asInstanceOf` to
   * explicitly coerce types. It is unsafe.
   */
  def unsafeForce[L, H >: L, A >: L <: H, B >: L <: H]: Liskov[L, H, A, B] =
    refl[Any].asInstanceOf[Liskov[L, H, A, B]]
}
