package scalaz
package scalacheck

import org.scalacheck.{Arbitrary, Prop, Properties}
import Prop.forAll
import Scalaz._

/**
 * Scalacheck properties that should hold for instances of type classes defined in Scalaz Core.
 */
object ScalazProperties {

  object equal {
    def commutativity[A](implicit A: Equal[A], arb: Arbitrary[A]) = forAll(A.equalLaw.commutative _).label("commutativity")

    def reflexive[A](implicit A: Equal[A], arb: Arbitrary[A]) = forAll(A.equalLaw.reflexive _).label("reflexive")

    def transitive[A](implicit A: Equal[A], arb: Arbitrary[A]) = forAll(A.equalLaw.transitive _).label("transitive")
  }

  object semigroup {
    def associative[A](implicit A: Semigroup[A], eq: Equal[A], arb: Arbitrary[A]) = forAll(A.semigroupLaw.associative _).label("associative")
  }

  object monoid {
    def leftIdentity[A](implicit A: Monoid[A], eq: Equal[A], arb: Arbitrary[A]) = forAll(A.monoidLaw.leftIdentity _).label("leftIdentity")
    def rightIdentity[A](implicit A: Monoid[A], eq: Equal[A], arb: Arbitrary[A]) = forAll(A.monoidLaw.rightIdentity _).label("rightIdentity")
  }

  object group {
    def inverseExists[A](implicit A: Group[A], eq: Equal[A], arb: Arbitrary[A]) = forAll(A.groupLaw.inverseExists _).label("inverseExists")
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

  object monad {
    def rightIdentity[M[_], X](implicit M: Monad[M], e: Equal[M[X]], a: Arbitrary[M[X]]) =
      forAll(M.monadLaw.rightIdentity[X] _).label("Right identity")

    def leftIdentity[M[_], X, Y](implicit am: Monad[M], emy: Equal[M[Y]], ax: Arbitrary[X], af: Arbitrary[(X => M[Y])]) =
      forAll(am.monadLaw.leftIdentity[X, Y] _).label("Left identity")

    def associativity[M[_], X, Y, Z](implicit M: Monad[M], amx: Arbitrary[M[X]], af: Arbitrary[(X => M[Y])],
                                     ag: Arbitrary[(Y => M[Z])], emz: Equal[M[Z]]) =
      forAll(M.monadLaw.associativeBind[X, Y, Z] _).label("Associativity")
  }

  object applicative {
    def identity[F[_], X](implicit f: Applicative[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) =
      forAll(f.applicativeLaw.identity[X] _)

    def composition[F[_], X, Y, Z](implicit ap: Applicative[F], afx: Arbitrary[F[X]], au: Arbitrary[F[Y => Z]],
                                   av: Arbitrary[F[X => Y]], e: Equal[F[Z]]) = forAll(ap.applicativeLaw.composition[X, Y, Z] _)

    def homomorphism[F[_], X, Y](implicit ap: Applicative[F], ax: Arbitrary[X], af: Arbitrary[X => Y], e: Equal[F[Y]]) =
      forAll(ap.applicativeLaw.homomorphism[X, Y] _)

    def interchange[F[_], X, Y](implicit ap: Applicative[F], ax: Arbitrary[X], afx: Arbitrary[F[X => Y]], e: Equal[F[Y]]) =
      forAll(ap.applicativeLaw.interchange[X, Y] _)
  }

  object traverse {
    def identityTraverse[F[_], X, Y](implicit f: Traverse[F], afx: Arbitrary[F[X]], axy: Arbitrary[X => Y], ef: Equal[F[Y]]) =
      forAll(f.traverseLaw.identityTraverse[X, Y] _)

    def purity[F[_], G[_], X](implicit f: Traverse[F], afx: Arbitrary[F[X]], G: Applicative[G], ef: Equal[G[F[X]]]) =
      forAll(f.traverseLaw.purity[G, X] _)

    def sequentialFusion[F[_], N[_], M[_], A, B, C](implicit fa: Arbitrary[F[A]], amb: Arbitrary[A => M[B]], bnc: Arbitrary[B => N[C]],
                                                      F: Traverse[F], N: Applicative[N], M: Applicative[M], MN: Equal[M[N[F[C]]]]): Prop =
      forAll(F.traverseLaw.sequentialFusion[N, M, A, B, C] _)

    def naturality[F[_], N[_], M[_], A](nat: (M ~> N))
                                       (implicit fma: Arbitrary[F[M[A]]], F: Traverse[F], N: Applicative[N], M: Applicative[M], NFA: Equal[N[F[A]]]): Prop =
      forAll(F.traverseLaw.naturality[N, M, A](nat) _)

    def parallelFusion[F[_], N[_], M[_], A, B](implicit fa: Arbitrary[F[A]], amb: Arbitrary[A => M[B]], anb: Arbitrary[A => N[B]],
                                               F: Traverse[F], N: Applicative[N], M: Applicative[M], MN: Equal[(M[F[B]], N[F[B]])]): Prop =
      forAll(F.traverseLaw.parallelFusion[N, M, A, B] _)
  }

  class ApplicativeLaws[F[_]](implicit F: Applicative[F], af: Arbitrary[F[Int]], aff: Arbitrary[F[Int => Int]], e: Equal[F[Int]])
    extends Properties("Applicative Laws") {
    property("identity") = applicative.identity[F, Int]
    property("composition") = applicative.composition[F, Int, Int, Int]
    property("homomorphism") = applicative.homomorphism[F, Int, Int]
    property("interchange") = applicative.interchange[F, Int, Int]
  }

  class MonadLaws[M[_]](implicit a: Monad[M], am: Arbitrary[M[Int]], af: Arbitrary[Int => M[Int]], e: Equal[M[Int]])
    extends Properties("Monad Laws") {
    property("Right identity") = monad.rightIdentity[M, Int]
    property("Left identity") = monad.leftIdentity[M, Int, Int]
    property("Associativity") = monad.associativity[M, Int, Int, Int]
  }

}
