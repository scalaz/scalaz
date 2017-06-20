package scalaz
package data

import Prelude._

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
    * Substitution into a contravariant context.
    *
    * @see [[substCo]]
    */
  def substCt[F[-_ >: L <: H]](fb: F[B]): F[A] = {
    type f[+x >: L <: H] = F[x] => F[A]
    substCo[f](identity[F[A]])(fb)
  }

  /**
    * Substitution into a covariant context.
    *
    * @see [[substCt]]
    */
  def substCo[F[+_ >: L <: H]](fa: F[A]): F[B]

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
  final def coerce(a: A): B =
    substCo[λ[`+α` => α]](a)

  //  /**
  //    * Given `A <~< B` we can prove that `F[A] <~< F[B]` for any
  //    * covariant `F[+_]`.
  //    *
  //    * @see [[liftCt]]
  //    */
  //  final def liftCo[F[+_]]: F[A] <~< F[B] = {
  //    type f[-α] = Liskov[F[α], F[B]]
  //    substCt[f](refl)
  //  }
  //
  //  /**
  //    * Given `A <~< B` we can prove that `F[B] <~< F[B]` for any
  //    * contravariant `F[-_]`.
  //    *
  //    * @see [[liftCo]]
  //    */
  //  final def liftCt[F[-_]]: F[B] <~< F[A] = {
  //    type f[+α] = F[α] <~< F[A]
  //    substCo[f](refl)
  //  }

  /**
    * A value of `A <~< B` is always sufficient to produce a similar [[<:<]]
    * value.
    */
  final def toPredef: A <:< B = {
    type f[-α] = α <:< B
    substCt[f](implicitly[B <:< B])
  }
}

object Liskov {
  def apply[L, H >: L, A >: L <: H, B >: L <: H](implicit ab: Liskov[L, H, A, B]): Liskov[L, H, A, B] = ab

  final case class Refl[A]() extends Liskov[A, A, A, A] {
//    def fix[L1 <: A, H1 >: A, A1 >: L1 <: A, B1 >: A <: H1]: Liskov1[L1, H1, A1, B1] =
//      Liskov1.proved[L1, H1, A1, B1, A1, B1](Leibniz.refl[A1], Leibniz.refl[B1])

    def substCo[F[+_ >: A <: A]](x: F[A]): F[A] = x
  }
  private[this] val anyRefl: Liskov[Any, Any, Any, Any] = Refl[Any]()

  /**
    * Unsafe coercion between types. `unsafeForce` abuses `asInstanceOf` to
    * explicitly coerce types. It is unsafe.
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def unsafeForce[L, H >: L, A >: L <: H, B >: L <: H]: Liskov[L, H, A, B] =
    anyRefl.asInstanceOf[Liskov[L, H, A, B]]

  /**
    * Subtyping relation is reflexive.
    */
  implicit def refl[A]: Liskov[A, A, A, A] = unsafeForce[A, A, A, A]

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
    * @see [[Liskov1.compose]]
    * @see [[Liskov1.andThen]]
    */
  def compose[L, H >: L, A >: L <: H, B >: L <: H, C >: L <: H]
  (bc: Liskov[L, H, B, C], ab: Liskov[L, H, A, B]): Liskov[L, H, A, C] =
    bc.substCo[λ[`+α >: L <: H` => Liskov[L, H, A, α]]](ab)

  /**
    * Subtyping is antisymmetric in theory (and in Dotty). Notice that this is
    * not true in Scala until [[https://issues.scala-lang.org/browse/SI-7278
    * SI-7278]] is fixed, so this function is marked unsafe.
    */
  def bracket[A, B, C](f: A <~< B, g: B <~< A): A === B =
    Is.unsafeForce[A, B]

  /**
    * It can be convenient to convert a [[<:<]] value into a `<~<` value.
    * This is not strictly valid as while it is almost certainly true that
    * `A <:< B` implies `A <~< B` it is not the case that you can create
    * evidence of `A <~< B` except via a coercion. Use responsibly.
    */
  def fromPredef[L, H >: L, A >: L <: H, B >: L <: H](eq: A <:< B): Liskov[L, H, A, B] =
    unsafeForce[L, H, A, B]
}