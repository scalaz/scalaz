package scalaz
package scalacheck

import org.scalacheck.{Arbitrary, Prop, Properties}

object ScalazProperty extends ScalazPropertys

/**
 * Scalacheck properties that should hold for instances of type classes defined in Scalaz Core.
 */
trait ScalazPropertys {
  import data.*._
  import data.*->*._
  import wrap.Function1W._
  import Scalazing._
  import Prop.forAll

  object Equal {
    def commutativity[A: Equal : Arbitrary] =
      forAll((a1: A, a2: A) => (a1 ≟ a2) ≟ (a2 ≟ a1)).label("commutativity")

    def identity[A: Equal : Arbitrary] = forAll((a: A) => a ≟ a).label("identity")
  }

  object Semigroup {
    def associative[A: Semigroup : Equal : Arbitrary] =
      forAll((a1: A, a2: A, a3: A) => ((a1 ⊹ a2) ⊹ a3) ≟ (a1 ⊹ (a2 ⊹ a3))).label("associative")
  }

  object Monoid {
    def identity[A: Monoid : Equal : Arbitrary] = {
      implicit val z = implicitly[Monoid[A]].zero
      implicit val s = implicitly[Monoid[A]].semigroup
      forAll((a: A) => (a ⊹ ∅) ≟ a).label("identity")
    }
  }

  object Functor {
    def identity[F[_], X](implicit f: Functor[F],
                          afx: Arbitrary[F[X]],
                          ef: Equal[F[X]]) =
      forAll((a: F[X]) => (a ∘ Predef.identity) ≟ a).label("identity")

    def composition[F[_], X, Y, Z](implicit f: Functor[F],
                          af: Arbitrary[F[X]],
                          axy: Arbitrary[(X => Y)],
                          ayz: Arbitrary[(Y => Z)],
                          ef: Equal[F[Z]]) =
      forAll((a1: F[X], f1: (X => Y), f2: (Y => Z)) => ((a1 ∘ f1) ∘ f2) ≟ (a1 ∘ (f1 ∘ f2))).label("composition")
  }

  class MonadLaws[M[_]](implicit a: Monad[M], am: Arbitrary[M[Int]], af: Arbitrary[Int => M[Int]], e: Equal[M[Int]])
        extends Properties("Monad Laws") { 
    property("Right identity") = Monad.rightIdentity[M, Int]
    property("Left identity") = Monad.leftIdentity[M, Int, Int]
    property("Associativity") = Monad.associativity[M, Int, Int, Int]
  }
 
  object Monad {
    def rightIdentity[M[_], X](implicit m: Monad[M], e: Equal[M[X]], a: Arbitrary[M[X]]) = {
      implicit val b = m.bind
      implicit val p = m.pointed
      forAll((a: M[X]) => ((a >>= ((_: X).η))) ≟ a).label("Right identity")
    }
    
    def leftIdentity[M[_], X, Y](implicit am: Monad[M],
                         emy: Equal[M[Y]],
                         ax: Arbitrary[X],
                         af: Arbitrary[(X => M[Y])]) = {
      implicit val b = am.bind
      implicit val p = am.pointed
      forAll((a: X, f: X => M[Y]) => ((a.η >>= f) ≟ f(a))).label("Left identity")
    }

    def associativity[M[_], X, Y, Z](implicit mm: Monad[M],
                                     amx: Arbitrary[M[X]],
                                     af: Arbitrary[(X => M[Y])],
                                     ag: Arbitrary[(Y => M[Z])],
                                     emz: Equal[M[Z]]) = {
      implicit val b = mm.bind
      forAll((a: M[X], f: X => M[Y], g: Y => M[Z]) => ((a >>= f >>= g) ≟ (a >>= ((x) => f(x) >>= g)))).label("Associativity")
    }
  }

  class ApplicativeLaws[F[_]](implicit a: Applicative[F], af: Arbitrary[F[Int]], aff: Arbitrary[F[Int => Int]], e: Equal[F[Int]])
        extends Properties("Applicative Laws") {
    property("identity") = Applicative.identity[F, Int]
    property("composition") = Applicative.composition[F, Int, Int, Int]
    property("homomorphism") = Applicative.homomorphism[F, Int, Int]
    property("interchange") = Applicative.interchange[F, Int, Int]
  }

  object Applicative {
    def identity[F[_], X](implicit f: Applicative[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) = {
      implicit val p = f.pointed
      implicit val ap = f.applic
      forAll((v: F[X]) => v <*> ((x: X) => x).η[F] === v)
    }

    def composition[F[_], X, Y, Z](implicit ap: Applicative[F], 
                                   afx: Arbitrary[F[X]],
                                   au: Arbitrary[F[Y => Z]],
                                   av: Arbitrary[F[X => Y]],
                                   e: Equal[F[Z]]) = {
      implicit val p = ap.pointed
      implicit val apl = ap.applic
      forAll((u: F[Y => Z], v: F[X => Y], w: F[X]) =>
        (w <*> (v <*> (u <*> (((f: Y => Z) => (g: X => Y) => f compose g).η[F])))) === (w <*> v) <*> u)
    }

    def homomorphism[F[_], X, Y](implicit ap: Applicative[F], ax: Arbitrary[X], af: Arbitrary[X => Y], e: Equal[F[Y]]) = {
      implicit val p = ap.pointed
      implicit val apl = ap.applic
      forAll((f: X => Y, x: X) => x.η[F] <*> f.η[F] === f(x).η[F])
    }

    def interchange[F[_], X, Y](implicit ap: Applicative[F], ax: Arbitrary[X], afx: Arbitrary[F[X => Y]], e: Equal[F[Y]]) = {
      implicit val p = ap.pointed
      implicit val apl = ap.applic
      forAll((u: F[X => Y], y: X) => y.η[F] <*> u === u <*> ((f: X => Y) => f(y)).η[F])
    }
  }
}
