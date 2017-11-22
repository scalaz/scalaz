package scalaz
package data

// FIXME: remove once https://github.com/scala/bug/issues/10623 is fixed.
import com.github.ghik.silencer.silent

import Prelude.<~<
import scalaz.typeclass.{IsContravariant, IsCovariant}

/**
  * Liskov substitutability: A better `<:<`.
  *
  * `A As B` witnesses that `A` can be used in any negative context
  * that expects a `B`. (e.g. if you could pass an `A` into any function
  * that expects a `B`.)
  *
  * @see [[<~<]] `A <~< B` is a type synonym to `A As B`
  */
sealed abstract class As[-A, +B] { ab =>
  import As._

  /**
    * Substitution into a covariant context.
    *
    * @see [[substCt]]
    */
  def substCv[F[+_]](x: F[A]): F[B]

  /**
    * Substitution into a contravariant context.
    *
    * @see [[substCv]]
    */
  def substCt[F[-_]](x: F[B]): F[A] = {
    type f[+a] = F[a] => F[A]
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
  final def andThen[C](bc: B <~< C): A <~< C = {
    @silent type f[+x] = A <~< x
    bc.substCv[f](this)
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
    substCv[f](a)
  }

  /**
    * Given `A <~< B` we can prove that `F[A] <~< F[B]` for any
    * covariant `F[+_]`.
    *
    * @see [[liftCt]]
    */
  final def liftCv[F[+_]]: F[A] <~< F[B] = {
    @silent type f[+x] = F[A] <~< F[x]
    substCv[f](refl[F[A]])
  }

  /**
    * Given `A <~< B` we can prove that `F[A] <~< F[B]` for any
    * contravariant `F[-_]`.
    *
    * @see [[liftCv]]
    */
  final def liftCt[F[-_]]: F[B] <~< F[A] = {
    @silent type f[+x] = F[x] <~< F[A]
    substCv[f](refl)
  }

  /**
    * Given `A <~< B` we can convert `(X => A)` into `(X => B)`.
    */
  def onF[X](fa: X => A): X => B = {
    type f[+a] = X => a
    substCv[f](fa)
  }
}

object As extends AsInstances {
  def apply[A, B](implicit ev: A <~< B): A <~< B = ev

  private[this] final case class Refl[A]() extends (A <~< A) {
    def substCv[F[+_]](p: F[A]): F[A] = p
  }

  private[this] val refl_ : ∀[λ[α => α <~< α]] = ∀.of[λ[α => α <~< α]].from(new Refl)

  /**
    * Subtyping relation is reflexive.
    */
  implicit def refl[A]: (A <~< A) = Forall.toForallOps(refl_).apply[A]

  /**
    * Reify Scala's subtyping relationship into an evidence value.
    */
  implicit def reify[A, B >: A]: A <~< B = refl[A]

  implicit final class AsOps[A, B](val ab: As[A, B]) extends AnyVal {
    def liftCoF[F[_]](implicit F: IsCovariant[F]): F[A] As F[B] =
      F.liftLiskov(ab)

    def liftCtF[F[_]](implicit F: IsContravariant[F]): F[B] As F[A] =
      F.liftLiskov(ab)

    def substCvF[F[_]](fa: F[A])(implicit F: IsCovariant[F]): F[B] = {
      type f[+x] = x
      F.substCv[f, A, B](fa)(ab)
    }

    def substCtF[F[_]](fb: F[B])(implicit F: IsContravariant[F]): F[A] = {
      type f[+x] = x
      F.substCv[f, A, B](fb)(ab)
    }
  }

  /**
    * Given `A <:< B`, prove `A <~< B`
    */
  def fromPredef[A, B](ev: A <:< B): A <~< B = {
    val _ = ev
    unsafeForce[A, B]
  }

  /**
    * Unsafe coercion between types. `unsafeForce` abuses `asInstanceOf` to
    * explicitly coerce types. It is unsafe.
    */
  def unsafeForce[A, B]: A <~< B =
    refl[Any].asInstanceOf[A <~< B]
}
