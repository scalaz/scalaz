package scalaz
package data

import Prelude.{>~>, <~<, ===}
import scala.annotation.unchecked.{uncheckedVariance => uV }
import scalaz.typeclass.{Contravariant, Functor}
import Liskov.refl

sealed abstract class Liskov[-A, +B] private[Liskov] () { ab =>
  def subst[F[-_]](fb: F[B]): F[A]

  def substCt[F[-_]](fb: F[B]): F[A] =
    subst[F](fb)

  def substCo[F[+_]](fa: F[A]): F[B] = {
    type f[-α] = F[α] => F[B]
    substCt[f](identity[F[B]]).apply(fa)
  }

  def apply(a: A): B =
    coerce(a)

  /**
    * Subtyping is transitive and its witnesses can be composed in a
    * chain much like functions.
    */
  def andThen[C](bc: B <~< C): A <~< C = {
    type f[-α] = α <~< C
    ab.substCt[f](bc)
  }

  /**
    * Subtyping is transitive and its witnesses can be composed in a
    * chain much like functions.
    *
    * @see [[andThen]]
    */
  def compose[Z](za: Z <~< A): Z <~< B =
    za.andThen(ab)

  /**
    * Substitution on identity brings about a direct coercion function
    * of the same form that [[<:<]] provides.
    *
    * @see [[apply]]
    */
  def coerce(a: A): B =
    substCo[λ[`+α` => α]](a)

  def liftCo[F[+_]]: F[A] <~< F[B] = {
    type f[-α] = F[α] <~< F[B]
    substCt[f](refl)
  }

  def liftCt[F[-_]]: F[B] <~< F[A] = {
    type f[+α] = F[α] <~< F[A]
    substCo[f](refl)
  }

  /**
    * A value of `A <~< B` is always sufficient to produce a similar [[<:<]]
    * value.
    */
  def toPredef: A <:< B = {
    type f[-α] = α <:< B
    substCt[f](implicitly[B <:< B])
  }
}

object Liskov {
  private[this] final case class Refl[A]() extends Liskov[A, A] {
    def subst[F[-_]](fa: F[A]): F[A] = fa
  }
  private[this] val anyRefl: Any <~< Any = Refl[Any]()

  // FIXME: This optimization is safe:
  // def refl[A]: A <~< A = reflAny.asInstanceOf[A <~< A]
  def refl[A]: A <~< A = Refl[A]()

  /**
    * Lift Scala's subtyping relationship
    */
  def isa[A, B >: A]: A <~< B = refl

  /**
    *
    * @see http://typelevel.org/blog/2014/03/09/liskov_lifting.html
    */
  def liftCtF[F[_]: Contravariant, A, B](ab: A <~< B): F[B] <~< F[A] =
    ab.asInstanceOf[F[B] <~< F[A]]

  /**
    *
    * @see http://typelevel.org/blog/2014/03/09/liskov_lifting.html
    */
  def liftCoF[F[_]: Functor, A, B](ab: A <~< B): F[A] <~< F[B] =
    ab.asInstanceOf[F[A] <~< F[B]]

  /**
    * Subtyping is transitive
    */
  def trans[A, B, C](f: B <~< C, g: A <~< B): A <~< C =
    g.subst[λ[`-α` => α <~< C]](f)

  /**
    * Subtyping is antisymmetric.
    */
  def bracket[A, B, C](f: A <~< B, g: B <~< A): A === B =
    Leibniz.anyRefl.asInstanceOf[A === B]

  /**
    * It can be convenient to convert a [[<:<]] value into a `<~<` value.
    * This is not strictly valid as while it is almost certainly true that
    * `A <:< B` implies `A <~< B` it is not the case that you can create
    * evidence of `A <~< B` except via a coercion. Use responsibly.
    */
  def unsafeFromPredef[A, B](eq: A <:< B): A <~< B =
    anyRefl.asInstanceOf[A <~< B]
}
