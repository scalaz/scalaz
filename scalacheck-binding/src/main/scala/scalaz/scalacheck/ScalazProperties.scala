package scalaz
package scalacheck

import org.scalacheck.{Arbitrary, Prop, Properties}

/**
 * Scalacheck properties that should hold for instances of type classes defined in Scalaz Core.
 */
object ScalazProperties {
  import Scalaz._
  import Prop.forAll

  object equal {
    def commutativity[A](implicit A: Equal[A], arb: Arbitrary[A]) = forAll(A.equalLaw.commutative _).label("commutativity")

    def reflexive[A](implicit A: Equal[A], arb: Arbitrary[A]) = forAll(A.equalLaw.reflexive _).label("reflexive")

    def transitive[A](implicit A: Equal[A], arb: Arbitrary[A]) = forAll(A.equalLaw.transitive _).label("transitive")
  }

  object semigroup {
    def associative[A](implicit A: Semigroup[A], eq: Equal[A], arb: Arbitrary[A]) = forAll(A.semigroupLaw.associative _).label("associative")
  }

  object monoid {
    def identity[A](implicit A: Monoid[A], eq: Equal[A], arb: Arbitrary[A]) = forAll(A.monoidLaw.identity _).label("identity")
  }

  object functor {
    def identity[F[_], X](implicit F: Functor[F],
                          afx: Arbitrary[F[X]],
                          ef: Equal[F[X]]) =
      forAll(F.functorLaw.identity[X] _).label("identity")

    def associative[F[_], X, Y, Z](implicit F: Functor[F],
                          af: Arbitrary[F[X]],
                          axy: Arbitrary[(X => Y)],
                          ayz: Arbitrary[(Y => Z)],
                          ef: Equal[F[Z]]) =
      forAll(F.functorLaw.associative[X, Y, Z] _).label("associative")
  }

  class MonadLaws[M[_]](implicit a: Monad[M], am: Arbitrary[M[Int]], af: Arbitrary[Int => M[Int]], e: Equal[M[Int]])
        extends Properties("Monad Laws") {
    property("Right identity") = monad.rightIdentity[M, Int]
    property("Left identity") = monad.leftIdentity[M, Int, Int]
    property("Associativity") = monad.associativity[M, Int, Int, Int]
  }

  object monad {
    def rightIdentity[M[_], X](implicit M: Monad[M], e: Equal[M[X]], a: Arbitrary[M[X]]) =
      forAll(M.monadLaw.rightIdentity[X] _).label("Right identity")

    def leftIdentity[M[_], X, Y](implicit am: Monad[M], emy: Equal[M[Y]], ax: Arbitrary[X], af: Arbitrary[(X => M[Y])]) =
      forAll(am.monadLaw.leftIdentity[X, Y] _).label("Left identity")

    def associativity[M[_], X, Y, Z](implicit M: Monad[M], amx: Arbitrary[M[X]], af: Arbitrary[(X => M[Y])],
                                     ag: Arbitrary[(Y => M[Z])], emz: Equal[M[Z]]) =
      forAll(M.monadLaw.associativeBind[X, Y, Z] _).label("Associativity")
  }

  class ApplicativeLaws[F[_]](implicit F: Applicative[F], af: Arbitrary[F[Int]], aff: Arbitrary[F[Int => Int]], e: Equal[F[Int]])
        extends Properties("Applicative Laws") {
    property("identity") = forAll(F.applicativeLaw.identity[Int] _)
    property("composition") = forAll(F.applicativeLaw.composition[Int, Int, Int] _)
    property("homomorphism") = forAll(F.applicativeLaw.homomorphism[Int, Int] _)
    property("interchange") = forAll(F.applicativeLaw.interchange[Int, Int] _)
  }
}
