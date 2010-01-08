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

  object Functor {
    // todo
//    def identity[F[_] : Functor : Equal : Arbitrary, X: Equal : Arbitrary] =
//      forAll((a: F[X]) => (a ∘ Predef.identity) ≟ a).label("identity")
//
//    def associative[A[_] : Functor : Equal : Arbitrary, XEqual: Arbitrary] =
//      forAll((a1: A[X], a2: A[X], a3: A[X]) => ((a1 ∘ a2) ∘ a3) ≟ ((a1 ∘ a2) ∘ a3)).label("associative")
  }

  object Monad {
    def identity[M[_], X](implicit m: Monad[M], e: Equal[M[X]], a: Arbitrary[M[X]]) =
      forAll((a: M[X]) => ((a ∗ ((_: X).η))) ≟ a).label("identity")
    
    def unit[M[_], X, Y](implicit am: Monad[M],
                         emy: Equal[M[Y]],
                         ax: Arbitrary[X],
                         af: Arbitrary[(X => M[Y])]) =
      forAll((a: X, f: X => M[Y]) => ((a.η ∗ f) ≟ f(a))).label("unit")

    def composition[M[_], X, Y, Z](implicit mm: Monad[M],
                                   amx: Arbitrary[M[X]],
                                   af: Arbitrary[(X => M[Y])],
                                   ag: Arbitrary[(Y => M[Z])],
                                   emz: Equal[M[Z]]) =
      forAll((a: M[X], f: X => M[Y], g: Y => M[Z]) => ((a ∗ f ∗ g) ≟ (a ∗ ((x) => f(x) ∗ g)))).label("composition")
  }
}