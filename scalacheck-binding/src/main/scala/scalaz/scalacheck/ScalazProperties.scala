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
    def commutativity[A](implicit A: Equal[A], arb: Arbitrary[A]) = forAll(A.equalLaw.commutative _)

    def reflexive[A](implicit A: Equal[A], arb: Arbitrary[A]) = forAll(A.equalLaw.reflexive _)

    def transitive[A](implicit A: Equal[A], arb: Arbitrary[A]) = forAll(A.equalLaw.transitive _)

    def naturality[A](implicit A: Equal[A], arb: Arbitrary[A]) = forAll(A.equalLaw.naturality _)

    def laws[A](implicit A: Equal[A], arb: Arbitrary[A]) = new Properties("equal") {
      property("commutativity") = commutativity[A]
      property("reflexive") = reflexive[A]
      property("transitive") = transitive[A]
      property("naturality") = naturality[A]
    }
  }

  object order {
    def transitiveOrder[A](implicit A: Order[A], arb: Arbitrary[A]) = forAll(A.orderLaw.transitiveOrder _)

    def orderAndEqualConsistent[A](implicit A: Order[A], arb: Arbitrary[A]) = forAll(A.orderLaw.orderAndEqualConsistent _)

    import scala.math.{Ordering => SOrdering}

    def scalaOrdering[A: Order: SOrdering: Arbitrary] = forAll((a1: A, a2: A) => Order[A].order(a1, a2) == Ordering.fromInt(SOrdering[A].compare(a1, a2)))

    def laws[A](implicit A: Order[A], arb: Arbitrary[A]) = new Properties("order") {
      include(equal.laws[A])
      property("transitive order") = transitiveOrder[A]
      property("order and equal consistent") = orderAndEqualConsistent[A]
    }
  }

  object semigroup {
    def associative[A](implicit A: Semigroup[A], eqa: Equal[A], arb: Arbitrary[A]) = forAll(A.semigroupLaw.associative _)

    def laws[A](implicit A: Semigroup[A], eqa: Equal[A], arb: Arbitrary[A]) = new Properties("semigroup") {
      property("associative") = associative[A]
    }
  }

  object monoid {
    def leftIdentity[A](implicit A: Monoid[A], eqa: Equal[A], arb: Arbitrary[A]) = forAll(A.monoidLaw.leftIdentity _)

    def rightIdentity[A](implicit A: Monoid[A], eqa: Equal[A], arb: Arbitrary[A]) = forAll(A.monoidLaw.rightIdentity _)

    def laws[A](implicit A: Monoid[A], eqa: Equal[A], arb: Arbitrary[A]) = new Properties("monoid") {
      include(semigroup.laws[A])
      property("left identity") = leftIdentity[A]
      property("right identity") = rightIdentity[A]
    }
  }

  object group {
    def inverseExists[A](implicit A: Group[A], eqa: Equal[A], arb: Arbitrary[A]) = forAll(A.groupLaw.inverseExists _)

    def laws[A](implicit A: Group[A], eqa: Equal[A], arb: Arbitrary[A]) = new Properties("group") {
      include(monoid.laws[A])
      property("inverse exists") = inverseExists[A]
    }
  }

  object functor {
    def identity[F[_], X](implicit F: Functor[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) =
      forAll(F.functorLaw.identity[X] _)

    def associative[F[_], X, Y, Z](implicit F: Functor[F], af: Arbitrary[F[X]], axy: Arbitrary[(X => Y)],
                                   ayz: Arbitrary[(Y => Z)], ef: Equal[F[Z]]) =
      forAll(F.functorLaw.associative[X, Y, Z] _)

    def laws[F[_]](implicit F: Functor[F], af: Arbitrary[F[Int]], axy: Arbitrary[(Int => Int)],
                   ef: Equal[F[Int]]) = new Properties("functor") {
      property("identity") = identity[F, Int]
      property("associative") = associative[F, Int, Int, Int]
    }
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

    def laws[F[_]](implicit F: Applicative[F], af: Arbitrary[F[Int]],
                   aff: Arbitrary[F[Int => Int]], e: Equal[F[Int]]) = new Properties("applicative") {
      include(functor.laws[F])
      property("identity") = applicative.identity[F, Int]
      property("composition") = applicative.composition[F, Int, Int, Int]
      property("homomorphism") = applicative.homomorphism[F, Int, Int]
      property("interchange") = applicative.interchange[F, Int, Int]
    }
  }

  object monad {
    def rightIdentity[M[_], X](implicit M: Monad[M], e: Equal[M[X]], a: Arbitrary[M[X]]) =
      forAll(M.monadLaw.rightIdentity[X] _)

    def leftIdentity[M[_], X, Y](implicit am: Monad[M], emy: Equal[M[Y]], ax: Arbitrary[X], af: Arbitrary[(X => M[Y])]) =
      forAll(am.monadLaw.leftIdentity[X, Y] _)

    def associativity[M[_], X, Y, Z](implicit M: Monad[M], amx: Arbitrary[M[X]], af: Arbitrary[(X => M[Y])],
                                     ag: Arbitrary[(Y => M[Z])], emz: Equal[M[Z]]) =
      forAll(M.monadLaw.associativeBind[X, Y, Z] _)

    def laws[M[_]](implicit a: Monad[M], am: Arbitrary[M[Int]],
                   af: Arbitrary[Int => M[Int]], ag: Arbitrary[M[Int => Int]], e: Equal[M[Int]]) = new Properties("monad") {
      include(applicative.laws[M])

      property("right identity") = monad.rightIdentity[M, Int]
      property("left identity") = monad.leftIdentity[M, Int, Int]
      property("associativity") = monad.associativity[M, Int, Int, Int]

    }
  }


  object copointed {
    def laws[M[_]](implicit a: CoPointed[M], am: Arbitrary[M[Int]],
                   af: Arbitrary[Int => Int], e: Equal[M[Int]]) = new Properties("copointed") {
      include(functor.laws[M])
    }
  }

  object comonad {
    def cobindLeftIdentity[F[_], A](implicit F: CoMonad[F], F0: Equal[F[A]], fa: Arbitrary[F[A]]) =
      forAll(F.coMonadLaw.cobindLeftIdentity[A] _)

    def cobindRightIdentity[F[_], A, B](implicit F: CoMonad[F], F0: Equal[B], fa: Arbitrary[F[A]], f: Arbitrary[F[A] => B]) =
      forAll(F.coMonadLaw.cobindRightIdentity[A, B] _)

    def cobindAssociative[F[_], A, B, C, D](implicit F: CoMonad[F], D: Equal[D], fa: Arbitrary[F[A]],
                                            f: Arbitrary[F[A] => B], g: Arbitrary[F[B] => C], h: Arbitrary[F[C] => D]) =
      forAll(F.coMonadLaw.cobindAssociative[A, B, C, D] _)

    def laws[F[_]](implicit a: CoMonad[F], am: Arbitrary[F[Int]],
                   af: Arbitrary[F[Int] => Int], e: Equal[F[Int]]) = new Properties("comonad") {
      include(copointed.laws[F])
      property("cobind left identity") = cobindLeftIdentity[F, Int]
      property("cobind right identity") = cobindRightIdentity[F, Int, Int]
      property("cobind associative") = cobindAssociative[F, Int, Int, Int, Int]
    }
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

    def laws[F[_]](implicit fa: Arbitrary[F[Int]], amb: Arbitrary[Int => Option[Int]], anb: Arbitrary[Int => Stream[Int]],
                   F: Traverse[F], EF: Equal[F[Int]], MN: Equal[(Option[F[Int]], Stream[F[Int]])]) =
      new Properties("traverse") {
        property("identity traverse") = identityTraverse[F, Int, Int]

        import std.list._, std.option._, std.stream._, std.anyVal._

        property("purity.option") = purity[F, Option, Int]
        property("purity.stream") = purity[F, Stream, Int]

        property("sequential fusion") = sequentialFusion[F, Option, List, Int, Int, Int]
      }
  }

  object plus {
    def associative[F[_], X](implicit f: Plus[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) =
      forAll(f.plusLaw.associative[X] _)

    def laws[F[_]](implicit F: Plus[F], afx: Arbitrary[F[Int]], ef: Equal[F[Int]]) = new Properties("plus") {
      include(semigroup.laws[F[Int]](F.semigroup[Int], implicitly, implicitly))
      property("associative") = associative[F, Int]
    }
  }

  object plusEmpty {
    def leftPlusIdentity[F[_], X](implicit f: PlusEmpty[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) =
      forAll(f.plusEmptyLaw.leftPlusIdentity[X] _)

    def rightPlusIdentity[F[_], X](implicit f: PlusEmpty[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) =
      forAll(f.plusEmptyLaw.rightPlusIdentity[X] _)

    def laws[F[_]](implicit F: PlusEmpty[F], afx: Arbitrary[F[Int]], af: Arbitrary[Int => Int], ef: Equal[F[Int]]) = new Properties("plusEmpty") {
      include(plus.laws[F])
      include(monoid.laws[F[Int]](F.monoid[Int], implicitly, implicitly))
      property("left plus identity") = leftPlusIdentity[F, Int]
      property("right plus identity") = rightPlusIdentity[F, Int]
    }
  }

  object monadPlus {
    def emptyMap[F[_], X](implicit f: MonadPlus[F], afx: Arbitrary[X => X], ef: Equal[F[X]]) =
      forAll(f.monadPlusLaw.emptyMap[X] _)

    def leftZero[F[_], X](implicit F: MonadPlus[F], afx: Arbitrary[X => F[X]], ef: Equal[F[X]]) =
      forAll(F.monadPlusLaw.leftZero[X] _)

    def rightZero[F[_], X](implicit F: MonadPlus[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) =
      forAll(F.strongMonadPlusLaw.rightZero[X] _)

    def laws[F[_]](implicit F: MonadPlus[F], afx: Arbitrary[F[Int]], afy: Arbitrary[F[Int => Int]], ef: Equal[F[Int]]) = new Properties("monad plus") {
      include(monad.laws[F])
      include(plusEmpty.laws[F])
      property("empty map") = emptyMap[F, Int]
      property("left zero") = leftZero[F, Int]
    }
    def strongLaws[F[_]](implicit F: MonadPlus[F], afx: Arbitrary[F[Int]], afy: Arbitrary[F[Int => Int]], ef: Equal[F[Int]]) = new Properties("monad plus") {
      include(laws[F])
      property("right zero") = rightZero[F, Int]
    }
  }

  object compose {
    def associative[=>:[_, _], A, B, C, D](implicit ab: Arbitrary[A =>: B], bc: Arbitrary[B =>: C],
                                           cd: Arbitrary[C =>: D], C: Compose[=>:], E: Equal[A =>: D]) =
      forAll(C.composeLaw.associative[A, B, C, D] _)

    def laws[=>:[_, _]](implicit C: Category[=>:], AB: Arbitrary[Int =>: Int], E: Equal[Int =>: Int]) = new Properties("category") {
      property("associative") = associative[=>:, Int, Int, Int, Int]
      include(semigroup.laws[Int =>: Int](C.semigroup[Int], implicitly, implicitly))
    }
  }

  object category {
    def leftIdentity[=>:[_, _], A, B](implicit ab: Arbitrary[A =>: B], C: Category[=>:], E: Equal[A =>: B]) =
      forAll(C.categoryLaw.leftIdentity[A, B] _)

    def rightIdentity[=>:[_, _], A, B](implicit ab: Arbitrary[A =>: B], C: Category[=>:], E: Equal[A =>: B]) =
      forAll(C.categoryLaw.rightIdentity[A, B] _)

    def laws[=>:[_, _]](implicit C: Category[=>:], AB: Arbitrary[Int =>: Int], E: Equal[Int =>: Int]) = new Properties("category") {
      include(compose.laws[=>:])
      property("left identity") = leftIdentity[=>:, Int, Int]
      property("right identity") = rightIdentity[=>:, Int, Int]
      include(monoid.laws[Int =>: Int](C.monoid[Int], implicitly, implicitly))
    }
  }

  object bifunctor {
    def laws[F[_, _]](implicit F: BiFunctor[F], E: Equal[F[Int, Int]], af: Arbitrary[F[Int, Int]],
                      axy: Arbitrary[(Int => Int)]) = new Properties("bifunctor") {
      include(functor.laws[({type λ[α]=F[α, Int]})#λ](F.leftFunctor[Int], implicitly, implicitly, implicitly))
      include(functor.laws[({type λ[α]=F[Int, α]})#λ](F.rightFunctor[Int], implicitly, implicitly, implicitly))
    }
  }

  object lens {
    def identity[A, B](l: Lens[A, B])(implicit A: Arbitrary[A], EA: Equal[A]) = forAll(l.lensLaw.identity _)
    def retention[A, B](l: Lens[A, B])(implicit A: Arbitrary[A], B: Arbitrary[B], EB: Equal[B]) = forAll(l.lensLaw.retention _)
    def doubleSet[A, B](l: Lens[A, B])(implicit A: Arbitrary[A], B: Arbitrary[B], EB: Equal[A]) = forAll(l.lensLaw.doubleSet _)

    def laws[A, B](l: Lens[A, B])(implicit A: Arbitrary[A], B: Arbitrary[B], EA: Equal[A], EB: Equal[B]) = new Properties("lens") {
      property("identity") = identity[A, B](l)
      property("retention") = retention[A, B](l)
      property("doubleSet") = doubleSet[A, B](l)
    }
  }

  object metricSpace {
    def nonNegativity[F](implicit F: MetricSpace[F], af: Arbitrary[F]) = forAll(F.metricSpaceLaw.nonNegativity _)
    def identity[F](implicit F: MetricSpace[F], af: Arbitrary[F]) = forAll(F.metricSpaceLaw.identity _)
    def equality[F](implicit F: MetricSpace[F], E: Equal[F], af: Arbitrary[F]) = forAll(F.metricSpaceLaw.equality _)
    def symmetry[F](implicit F: MetricSpace[F], af: Arbitrary[F]) = forAll(F.metricSpaceLaw.symmetry _)
    def triangleInequality[F](implicit F: MetricSpace[F], af: Arbitrary[F]) = forAll(F.metricSpaceLaw.triangleInequality _)
    def laws[F](implicit F: MetricSpace[F], E: Equal[F], af: Arbitrary[F]) = new Properties("metric space") {
      property("nonNegativity") = nonNegativity[F]
      property("identity") = identity[F]
      property("equality") = equality[F]
      property("symmetry") = symmetry[F]
      property("triangleInequality") = triangleInequality[F]
    }
  }
}
