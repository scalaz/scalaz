package scalaz
package data

import Prelude.{<~<, ===}
import scalaz.typeclass.{Contravariant, Functor}
import Liskov.refl

/**
  * Liskov substitutability: A better `<:<`.
  *
  * `Liskov[A, B]` witnesses that `A` can be used in any negative context
  * that expects a `B`. (e.g. if you could pass an `A` into any function
  * that expects a `B`.)
  *
  * @see [[<~<]] `A <~< B` is a type synonym to `Liskov[A, B]`
  */
sealed abstract class Liskov[-A, +B] private[Liskov] () { ab =>
  def subst[F[-_]](fb: F[B]): F[A]

  /**
    * Substitution into a contravariant context.
    *
    * @see [[substCo]]
    */
  final def substCt[F[-_]](fb: F[B]): F[A] =
    subst[F](fb)

  /**
    * Substitution into a covariant context.
    *
    * @see [[substCt]]
    */
  final def substCo[F[+_]](fa: F[A]): F[B] = {
    type f[-α] = F[α] => F[B]
    substCt[f](identity[F[B]]).apply(fa)
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
    type f[-α] = α <~< C
    ab.substCt[f](bc)
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
  final def coerce(a: A): B =
    substCo[λ[`+α` => α]](a)

  /**
    * Given `A <~< B` we can prove that `F[A] <~< F[B]` for any
    * covariant `F[+_]`.
    *
    * @see [[liftCt]]
    */
  final def liftCo[F[+_]]: F[A] <~< F[B] = {
    type f[-α] = F[α] <~< F[B]
    substCt[f](refl)
  }

  /**
    * Given `A <~< B` we can prove that `F[B] <~< F[B]` for any
    * contravariant `F[-_]`.
    *
    * @see [[liftCo]]
    */
  final def liftCt[F[-_]]: F[B] <~< F[A] = {
    type f[+α] = F[α] <~< F[A]
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

object Liskov {
  private[this] final case class Refl[A]() extends Liskov[A, A] {
    def subst[F[-_]](fa: F[A]): F[A] = fa
  }
  private[this] val anyRefl: Any <~< Any = Refl[Any]()

  /**
    * Subtyping relation is reflexive.
    */
  def refl[A]: A <~< A = {
    // FIXME: This optimization is safe:
    // reflAny.asInstanceOf[A <~< A]
    Refl[A]()
  }

  /**
    * Reify Scala's subtyping relationship into an evidence value.
    */
  def isa[A, B >: A]: A <~< B = refl

  /**
    * Given `A <~< B` and a contravariant functor `F[_]`, we can prove that
    * `F[B] <~< F[A]`. Notice that `F[B] <:< F[A]` may not hold in Scala, yet
    * such a substitution is always valid assuming that `F[_]` is a lawful
    * contravariant functor.
    *
    * @see [[http://typelevel.org/blog/2014/03/09/liskov_lifting.html
    *        When can Liskov be lifted?]]
    * @see [[liftCoF]]
    */
  def liftCtF[F[_]: Contravariant, A, B](ab: A <~< B): F[B] <~< F[A] =
    ab.asInstanceOf[F[B] <~< F[A]]

  /**
    * Given `A <~< B` and a covariant functor `F[_]`, we can prove that
    * `F[A] <~< F[B]`. Notice that `F[A] <:< F[B]` may not hold in Scala, yet
    * such a substitution is always valid assuming that `F[_]` is a lawful
    * covariant functor.
    *
    * @see [[http://typelevel.org/blog/2014/03/09/liskov_lifting.html
    *        When can Liskov be lifted?]]
    * @see [[liftCtF]]
    */
  def liftCoF[F[_]: Functor, A, B](ab: A <~< B): F[A] <~< F[B] =
    ab.asInstanceOf[F[A] <~< F[B]]

  /**
    * Subtyping is a transitive relation and its witnesses can be composed
    * in a chain much like functions.
    */
  def trans[A, B, C](bc: B <~< C, ab: A <~< B): A <~< C =
    ab.andThen(bc)

  /**
    * Subtyping is antisymmetric in theory (and in Dotty). Notice that this is
    * not true in Scala until [[https://issues.scala-lang.org/browse/SI-7278
    * SI-7278]] is fixed, so this function is marked unsafe.
    */
  def unsafeBracket[A, B, C](f: A <~< B, g: B <~< A): A === B =
    Leibniz.unsafeForce[A, B]

  /**
    * It can be convenient to convert a [[<:<]] value into a `<~<` value.
    * This is not strictly valid as while it is almost certainly true that
    * `A <:< B` implies `A <~< B` it is not the case that you can create
    * evidence of `A <~< B` except via a coercion. Use responsibly.
    */
  def unsafeFromPredef[A, B](eq: A <:< B): A <~< B =
    anyRefl.asInstanceOf[A <~< B]
}
