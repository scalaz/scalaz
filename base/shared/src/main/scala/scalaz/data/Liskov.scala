package scalaz
package data

import Prelude.<~<

/**
  * Liskov substitutability: A better `<:<`.
  *
  * `Liskov[A, B]` witnesses that `A` can be used in any negative context
  * that expects a `B`. (e.g. if you could pass an `A` into any function
  * that expects a `B`.)
  *
  * @see [[<~<]] `A <~< B` is a type synonym to `Liskov[A, B]`
  */
sealed abstract class Liskov[-L, +H >: L, -A >: L <: H, +B >: L <: H] private[Liskov]() { ab =>
  /**
    * Substitution into a covariant context.
    *
    * @see [[substCt]]
    */
  def substCv[F[+_ >: L <: H]](x: F[A]): F[B]

  /**
    * Substitution into a contravariant context.
    *
    * @see [[substCv]]
    */
  def substCt[F[-_ >: L <: H]](x: F[B]): F[A] = {
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
  final def andThen[L2 <: L, H2 >: H, C >: L2 <: H2]
  (bc: Liskov[L2, H2, B, C]): Liskov[L2, H2, A, C] =
    Liskov.compose[L2, H2, A, B, C](bc, ab)

  /**
    * Subtyping is transitive and its witnesses can be composed in a
    * chain much like functions.
    *
    * @see [[andThen]]
    */
  final def compose[L2 <: L, H2 >: H, Z >: L2 <: H2]
  (za: Liskov[L2, H2, Z, A]): Liskov[L2, H2, Z, B] =
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
  def apply[L, H >: L, A >: L <: H, B >: L <: H]
  (implicit ab: Liskov[L, H, A, B]): Liskov[L, H, A, B] = ab

  private[this] final case class Refl[A]() extends Liskov[A, A, A, A] {
    def substCv[F[+_ >: A <: A]](x: F[A]): F[A] = x
  }

  private[this] val reflAll = ∀.of[λ[α => Liskov[α, α, α, α]]].from(new Refl)

  /**
    * Subtyping relation is reflexive.
    */
  implicit def refl[A]: Liskov[A, A, A, A] = Forall.toForallOps(reflAll).apply[A]

  /**
    * Reify Scala's subtyping relationship into an evidence value.
    */
  implicit def reify[
    L, H >: L,
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
  def compose[L, H >: L, A >: L <: H, B >: L <: H, C >: L <: H]
  (bc: Liskov[L, H, B, C], ab: Liskov[L, H, A, B]): Liskov[L, H, A, C] =
    bc.substCv[λ[`+α >: L <: H` => Liskov[L, H, A, α]]](ab)

  /**
    * Subtyping is antisymmetric in theory (and in Dotty). Notice that this is
    * not true in Scala until [[https://issues.scala-lang.org/browse/SI-7278
    * SI-7278]] is fixed, so this function is marked unsafe.
    */
  def bracket[L, H >: L, A >: L <: H, B >: L <: H]
  (f: Liskov[L, H, A, B], g: Liskov[L, H, B, A]): Leibniz[L, H, A, B] = {
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