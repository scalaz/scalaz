package scalaz
package data

import Prelude.{<~<, ===}
import scalaz.typeclass.{Contravariant, Functor}

/**
  * Liskov substitutability: A better `<:<`.
  *
  * `A As B` witnesses that `A` can be used in any negative context
  * that expects a `B`. (e.g. if you could pass an `A` into any function
  * that expects a `B`.)
  *
  * @see [[<~<]] `A <~< B` is a type synonym to `A As B`
  */
sealed abstract class As[-A, +B] private[As]() { ab =>
  import As._

  /**
    * Substitution into a contravariant context.
    *
    * @see [[substCo]]
    */
  def substCt[F[-_]](fb: F[B]): F[A] = {
    type f[+x] = F[x] => F[A]
    substCo[f](identity[F[A]])(fb)
  }

  /**
    * Substitution into a covariant context.
    *
    * @see [[substCt]]
    */
  def substCo[F[+_]](fa: F[A]): F[B]

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
  final def andThen[C](bc: B As C): A As C = {
    type f[-α] = α As C
    ab.substCt[f](bc)
  }

  /**
    * Subtyping is transitive and its witnesses can be composed in a
    * chain much like functions.
    *
    * @see [[andThen]]
    */
  final def compose[Z](za: Z As A): Z As B =
    za.andThen(ab)

  /**
    * Substitution on identity brings about a direct coercion function
    * of the same form that [[<:<]] provides.
    *
    * @see [[apply]]
    */
  final def coerce(a: A): B =
    substCo[λ[`+α` => α]](a)

  /**
    * Given `A <~< B` we can prove that `F[A] <~< F[B]` for any
    * covariant `F[+_]`.
    *
    * @see [[liftCt]]
    */
  final def liftCo[F[+_]]: F[A] As F[B] = {
    type f[-α] = F[α] As F[B]
    substCt[f](refl)
  }

  /**
    * Given `A <~< B` we can prove that `F[A] <~< F[B]` for any
    * contravariant `F[-_]`.
    *
    * @see [[liftCo]]
    */
  final def liftCt[F[-_]]: F[B] As F[A] = {
    type f[+α] = F[α] As F[A]
    substCo[f](refl)
  }

  /**
    * A value of `A <~< B` is always sufficient to produce a similar [[<:<]]
    * value.
    */
  final def toPredef: A <:< B = {
    type f[-α] = α <:< B
    substCt[f](implicitly[B <:< B])
  }
}

object As {
  def apply[A, B](implicit ev: A As B): A As B = ev

  final case class Refl[A]() extends (A As A) {
    def substCo[F[+_]](fa: F[A]): F[A] = fa
  }
  private[this] val reflAny: Any As Any = new Refl[Any]()

  /**
    * Unsafe coercion between types. `unsafeForce` abuses `asInstanceOf` to
    * explicitly coerce types. It is unsafe.
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def unsafeForce[A, B]: A As B =
    reflAny.asInstanceOf[A As B]

  /**
    * Subtyping relation is reflexive.
    */
  implicit def refl[A]: A As A = unsafeForce[A, A]

  /**
    * Reify Scala's subtyping relationship into an evidence value.
    */
  implicit def reify[A, B >: A]: A As B = refl

  /**
    * Subtyping is antisymmetric in theory (and in Dotty). Notice that this is
    * not true in Scala until [[https://issues.scala-lang.org/browse/SI-7278
    * SI-7278]] is fixed.
    */
  def bracket[A, B, C](f: A As B, g: B As A): A === B =
    Is.unsafeForce[A, B]


  def pair[A1, B1, A2, B2] (eq1: A1 <~< B1, eq2: A2 <~< B2): Pair[A1, B1, A2, B2] =
    new Pair(eq1, eq2)

  final case class Pair[A1, B1, A2, B2] (eq1: A1 <~< B1, eq2: A2 <~< B2) {
    def liftCo[F[+_, +_]]: F[A1, A2] <~< F[B1, B2] = {
      type f1[+x] = F[A1, A2] <~< F[x, A2]
      type f2[+x] = F[A1, A2] <~< F[B1, x]
      eq2.substCo[f2](eq1.substCo[f1](refl[F[A1, A2]]))
    }
    def liftCt[F[-_, -_]]: F[B1, B2] <~< F[A1, A2] = {
      type f1[+x] = F[x, A2] <~< F[A1, A2]
      type f2[+x] = F[B1, x] <~< F[A1, A2]
      eq2.substCo[f2](eq1.substCo[f1](refl[F[A1, A2]]))
    }

    def substCo[F[+_, +_]](value: F[A1, A2]): F[B1, B2] =
      liftCo[F].apply(value)
    def substCt[F[-_, -_]](value: F[B1, B2]): F[A1, A2] =
      liftCt[F].apply(value)
  }

  implicit class AsOps1[B](val ab: As[Nothing, B]) extends AnyVal {
    def liftCoF[F[_]](implicit F: Functor[F]): F[Nothing] As F[B] =
      unsafeForce[F[Nothing], F[B]]

    def liftCtF[F[_]](implicit F: Contravariant[F]): F[B] As F[Nothing] =
      unsafeForce[F[B], F[Nothing]]

    def substCoF[F[_]](fa: F[Nothing])(implicit F: Functor[F]): F[B] =
      liftCoF[F].coerce(fa)

    def substCtF[F[_]](fb: F[B])(implicit F: Contravariant[F]): F[Nothing] =
      liftCtF[F].coerce(fb)
  }

  implicit class AsOps[A, B](val ab: As[A, B]) extends AnyVal {
    def liftCoF[F[_]](implicit F: Functor[F]): F[A] As F[B] =
      unsafeForce[F[A], F[B]]

    def liftCtF[F[_]](implicit F: Contravariant[F]): F[B] As F[A] =
      unsafeForce[F[B], F[A]]

    def substCoF[F[_]](fa: F[A])(implicit F: Functor[F]): F[B] =
      liftCoF[F].coerce(fa)

    def substCtF[F[_]](fb: F[B])(implicit F: Contravariant[F]): F[A] =
      liftCtF[F].coerce(fb)
  }

  /**
    * It can be convenient to convert a [[<:<]] value into a `<~<` value.
    * This is not strictly valid as while it is almost certainly true that
    * `A <:< B` implies `A <~< B` it is not the case that you can create
    * evidence of `A <~< B` except via a coercion. Use responsibly.
    */
  def fromPredef[A, B](eq: A <:< B): A As B =
    unsafeForce[A, B]
}