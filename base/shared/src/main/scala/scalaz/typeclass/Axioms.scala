package scalaz
package typeclass

import scalaz.Prelude._
import scalaz.data.{∀ => _, _}

/** */
object Axioms {
  /**
    * Subtyping is antisymmetric.
    *
    * a ~ b ⟷ a ≤ b ⋀ b ≤ a
    */
  def bracket[A, B](below: A <~< B, above: B <~< A): A === B = {
    val (_, _) = (below, above)
    Inhabited.unsafeForce[A === B].proved
  }

  def incomparable[F[_], A, B](ncmp: A >!< B): ¬¬[(F[A] === F[B]) \/ (F[A] >!< F[B])] = {
    val _ = ncmp
    Inhabited.unsafeForce
  }

  def subtyping[A, B](ev: A =!= B): ¬¬[(B </< A) \/ (A </< B) \/ (A >!< B)] = {
    val _ = ev
    Inhabited.unsafeForce
  }

  def parametricity[F[_]]: ¬¬[IsConstant[F] \/ IsInjective[F]] =
    Inhabited.unsafeForce

  def subtypeParametricity[F[_]](ev: IsInjective[F]): ¬¬[IsStrictlyContravariant[F] \/ IsStrictlyCovariant[F] \/ IsInvariant[F]] = {
    val _ = ev
    Inhabited.unsafeForce
  }

  /**
    *
    */
  def predefEq[A, B](eq: A =:= B): A === B = {
    val _ = eq
    Is.refl[A].asInstanceOf[A === B]
  }

  /**
    *
    */
  def predefConformity[A, B](eq: A <:< B): A <~< B = {
    val _ = eq
    As.refl[A].asInstanceOf[A <~< B]
  }
}
