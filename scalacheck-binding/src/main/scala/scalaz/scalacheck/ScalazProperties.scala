package scalaz
package scalacheck

import org.scalacheck.{Arbitrary, Prop}

/**
 * Scalacheck properties that should hold for instances of type classes defined in Scalaz Core.
 */
object ScalazProperties {
  import Scalaz._
  import Prop.forAll

  object Equal {
    def commutativity[A: Equal : Arbitrary] =
      forAll((a1: A, a2: A) => (a1 ≟ a2) ≟ (a2 ≟ a1)).label("commutativity")

    // todo better name for this?
    def identity[A: Equal : Arbitrary] = forAll((a: A) => a ≟ a).label("identity")
  }

  object Semigroup {
    def associative[A: Semigroup : Equal : Arbitrary] =
      forAll((a1: A, a2: A, a3: A) => ((a1 ⊹ a2) ⊹ a3) ≟ (a1 ⊹ (a2 ⊹ a3))).label("associative")
  }

  object Monoid {
    def identity[A: Monoid : Equal : Arbitrary] = forAll((a: A) => (a ⊹ ∅) ≟ a).label("identity")
  }
}