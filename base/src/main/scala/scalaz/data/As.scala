package scalaz
package data

import Prelude.{<~<, ===}
import typeclass.{ Functor, Contravariant }

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
    * Substitution into a covariant context.
    *
    * @see [[substCt]]
    */
  def substCo[F[+_]](fa: F[A]): F[B]

  /**
    * Substitution into a contravariant context.
    *
    * @see [[substCo]]
    */
  final def substCt[F[-_]](fb: F[B]): F[A] = {
    type f[+x] = F[x] => F[A]
    substCo[f](identity[F[A]])(fb)
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
  final def andThen[C](bc: B <~< C): A <~< C = {
    type f[+x] = A <~< x
    bc.substCo[f](this)
  }

  /**
    * Subtyping is transitive and its witnesses can be composed in a
    * chain much like functions.
    *
    * @see [[andThen]]
    */
  final def compose[Z](za: Z <~< A): Z <~< B =
    za.andThen(ab)

  /**
    * Substitution on identity brings about a direct coercion function
    * of the same form that [[<:<]] provides.
    *
    * @see [[apply]]
    */
  final def coerce(a: A): B = {
    type f[+x] = x
    substCo[f](a)
  }

  /**
    * Given `A <~< B` we can prove that `F[A] <~< F[B]` for any
    * covariant `F[+_]`.
    *
    * @see [[liftCt]]
    */
  final def liftCo[F[+_]]: F[A] <~< F[B] = {
    type f[+x] = F[A] <~< F[x]
    substCo[f](refl[F[A]])
  }

  /**
    * Given `A <~< B` we can prove that `F[A] <~< F[B]` for any
    * contravariant `F[-_]`.
    *
    * @see [[liftCo]]
    */
  final def liftCt[F[-_]]: F[B] <~< F[A] = {
    type f[+x] = F[x] <~< F[A]
    substCo[f](refl)
  }

  /**
    * Given `A <~< B` we can convert `(X => A)` into `(X => B)`.
    */
  def onF[X](fa: X => A): X => B = {
    type f[+a] = X => a
    substCo[f](fa)
  }

  /**
    * A value of `A <~< B` is always sufficient to produce a similar [[<:<]]
    * value.
    */
  final def toPredef: A <:< B = {
    type f[+a] = A <:< a
    substCo[f](implicitly[A <:< A])
  }
}

object As {
  def apply[A, B](implicit ev: A <~< B): A <~< B = ev

  final case class Refl[A]() extends (A <~< A) {
    def substCo[F[+_]](fa: F[A]): F[A] = fa
  }
  private[this] val reflAny: Any <~< Any = new Refl[Any]()

  /**
    * Unsafe coercion between types. `unsafeForce` abuses `asInstanceOf` to
    * explicitly coerce types. It is unsafe.
    */
  def unsafeForce[A, B]: A <~< B =
    reflAny.asInstanceOf[A <~< B]

  /**
    * Subtyping relation is reflexive.
    */
  implicit def refl[A]: A <~< A = unsafeForce[A, A]

  /**
    * Reify Scala's subtyping relationship into an evidence value.
    */
  implicit def reify[A, B >: A]: A <~< B = refl

  /**
    * Subtyping is antisymmetric in theory (and in Dotty). Notice that this is
    * not true in Scala until [[https://issues.scala-lang.org/browse/SI-7278
    * SI-7278]] is fixed.
    */
  def bracket[A, B, C](f: A <~< B, g: B <~< A): A === B =
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

  implicit class AsOps1[B](val ab: Nothing <~< B) extends AnyVal {
    def liftCoF[F[_]](implicit F: Functor[F]): F[Nothing] <~< F[B] =
      unsafeForce[F[Nothing], F[B]]

    def liftCtF[F[_]](implicit F: Contravariant[F]): F[B] <~< F[Nothing] =
      unsafeForce[F[B], F[Nothing]]

    def substCoF[F[_]](fa: F[Nothing])(implicit F: Functor[F]): F[B] =
      liftCoF[F].coerce(fa)

    def substCtF[F[_]](fb: F[B])(implicit F: Contravariant[F]): F[Nothing] =
      liftCtF[F].coerce(fb)
  }

  implicit class AsOps[A, B](val ab: A <~< B) extends AnyVal {
    def liftCoF[F[_]](implicit F: Functor[F]): F[A] <~< F[B] =
      unsafeForce[F[A], F[B]]

    def liftCtF[F[_]](implicit F: Contravariant[F]): F[B] <~< F[A] =
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
  def fromPredef[A, B](eq: A <:< B): A <~< B =
    unsafeForce[A, B]
}