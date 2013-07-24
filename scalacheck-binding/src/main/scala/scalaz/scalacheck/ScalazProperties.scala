package scalaz
package scalacheck

import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
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

  object enum {
    def succpred[A](implicit A: Enum[A], arb: Arbitrary[A]) = forAll(A.enumLaw.succpred _)

    def predsucc[A](implicit A: Enum[A], arb: Arbitrary[A]) = forAll(A.enumLaw.predsucc _)

    def minmaxpred[A](implicit A: Enum[A]): Prop = A.enumLaw.minmaxpred

    def minmaxsucc[A](implicit A: Enum[A]): Prop = A.enumLaw.minmaxsucc

    private val smallInt = Gen.choose(-100, 100)

    def succn[A](implicit A: Enum[A], arb: Arbitrary[A]) = forAll((x: A) => forAll(smallInt)(A.enumLaw.succn(x, _)))

    def predn[A](implicit A: Enum[A], arb: Arbitrary[A]) = forAll((x: A) => forAll(smallInt)(A.enumLaw.predn(x, _)))

    def succorder[A](implicit A: Enum[A], arb: Arbitrary[A]) = forAll(A.enumLaw.succorder _)

    def predorder[A](implicit A: Enum[A], arb: Arbitrary[A]) = forAll(A.enumLaw.predorder _)

    def laws[A](implicit A: Enum[A], arb: Arbitrary[A]) = new Properties("enum") {
      include(order.laws[A])
      property("predecessor then successor is identity") = succpred[A]
      property("successor then predecessor is identity") = predsucc[A]
      property("predecessor of the min is the max") = minmaxpred[A]
      property("successor of the max is the min") = minmaxsucc[A]
      property("n-successor is n-times successor") = succn[A]
      property("n-predecessor is n-times predecessor") = predn[A]
      property("successor is greater or equal") = succorder[A]
      property("predecessor is less or equal") = predorder[A]
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

  object invariantFunctor {
    def identity[F[_], X](implicit F: InvariantFunctor[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) =
      forAll(F.invariantFunctorLaw.invariantIdentity[X] _)

    def composite[F[_], X, Y, Z](implicit F: InvariantFunctor[F], af: Arbitrary[F[X]], axy: Arbitrary[(X => Y)],
                                   ayz: Arbitrary[(Y => Z)], ayx: Arbitrary[(Y => X)], azy: Arbitrary[(Z => Y)], ef: Equal[F[Z]]) =
      forAll(F.invariantFunctorLaw.invariantComposite[X, Y, Z] _)

    def laws[F[_]](implicit F: InvariantFunctor[F], af: Arbitrary[F[Int]], axy: Arbitrary[(Int => Int)],
                   ef: Equal[F[Int]]) = new Properties("invariantFunctor") {
      property("identity") = identity[F, Int]
      property("composite") = composite[F, Int, Int, Int]
    }
  }

  object functor {
    def identity[F[_], X](implicit F: Functor[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) =
      forAll(F.functorLaw.identity[X] _)

    def composite[F[_], X, Y, Z](implicit F: Functor[F], af: Arbitrary[F[X]], axy: Arbitrary[(X => Y)],
                                   ayz: Arbitrary[(Y => Z)], ef: Equal[F[Z]]) =
      forAll(F.functorLaw.composite[X, Y, Z] _)

    def laws[F[_]](implicit F: Functor[F], af: Arbitrary[F[Int]], axy: Arbitrary[(Int => Int)],
                   ef: Equal[F[Int]]) = new Properties("functor") {
      include(invariantFunctor.laws[F])
      property("identity") = identity[F, Int]
      property("composite") = composite[F, Int, Int, Int]
    }
  }

  object applicative {
    def identity[F[_], X](implicit f: Applicative[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) =
      forAll(f.applicativeLaw.identityAp[X] _)

    def composition[F[_], X, Y, Z](implicit ap: Applicative[F], afx: Arbitrary[F[X]], au: Arbitrary[F[Y => Z]],
                                   av: Arbitrary[F[X => Y]], e: Equal[F[Z]]) = forAll(ap.applicativeLaw.composition[X, Y, Z] _)

    def homomorphism[F[_], X, Y](implicit ap: Applicative[F], ax: Arbitrary[X], af: Arbitrary[X => Y], e: Equal[F[Y]]) =
      forAll(ap.applicativeLaw.homomorphism[X, Y] _)

    def interchange[F[_], X, Y](implicit ap: Applicative[F], ax: Arbitrary[X], afx: Arbitrary[F[X => Y]], e: Equal[F[Y]]) =
      forAll(ap.applicativeLaw.interchange[X, Y] _)

    def mapApConsistency[F[_], X, Y](implicit ap: Applicative[F], ax: Arbitrary[F[X]], afx: Arbitrary[X => Y], e: Equal[F[Y]]) =
      forAll(ap.applicativeLaw.mapLikeDerived[X, Y] _)

    def laws[F[_]](implicit F: Applicative[F], af: Arbitrary[F[Int]],
                   aff: Arbitrary[F[Int => Int]], e: Equal[F[Int]]) = new Properties("applicative") {
      include(functor.laws[F])
      property("identity") = applicative.identity[F, Int]
      property("composition") = applicative.composition[F, Int, Int, Int]
      property("homomorphism") = applicative.homomorphism[F, Int, Int]
      property("interchange") = applicative.interchange[F, Int, Int]
      property("map consistent with ap") = applicative.mapApConsistency[F, Int, Int]
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

    def bindApConsistency[M[_], X, Y](implicit M: Monad[M], amx: Arbitrary[M[X]],
                                      af: Arbitrary[M[X => Y]], emy: Equal[M[Y]]) =
      forAll(M.monadLaw.apLikeDerived[X, Y] _)

    def laws[M[_]](implicit a: Monad[M], am: Arbitrary[M[Int]],
                   af: Arbitrary[Int => M[Int]], ag: Arbitrary[M[Int => Int]], e: Equal[M[Int]]) = new Properties("monad") {
      include(applicative.laws[M])

      property("right identity") = monad.rightIdentity[M, Int]
      property("left identity") = monad.leftIdentity[M, Int, Int]
      property("associativity") = monad.associativity[M, Int, Int, Int]
      property("ap consistent with bind") = monad.bindApConsistency[M, Int, Int]

    }
  }

  object cobind {
    def cobindAssociative[F[_], A, B, C, D](implicit F: Cobind[F], D: Equal[D], fa: Arbitrary[F[A]],
                                            f: Arbitrary[F[A] => B], g: Arbitrary[F[B] => C], h: Arbitrary[F[C] => D]) =
      forAll(F.cobindLaw.cobindAssociative[A, B, C, D] _)

    def laws[F[_]](implicit a: Cobind[F], am: Arbitrary[F[Int]], e: Equal[F[Int]]) = new Properties("cobind") {
      include(functor.laws[F])
      property("cobind associative") = cobindAssociative[F, Int, Int, Int, Int]
    }
  }

  object comonad {
    def cobindLeftIdentity[F[_], A](implicit F: Comonad[F], F0: Equal[F[A]], fa: Arbitrary[F[A]]) =
      forAll(F.comonadLaw.cobindLeftIdentity[A] _)

    def cobindRightIdentity[F[_], A, B](implicit F: Comonad[F], F0: Equal[B], fa: Arbitrary[F[A]], f: Arbitrary[F[A] => B]) =
      forAll(F.comonadLaw.cobindRightIdentity[A, B] _)

    def laws[F[_]](implicit a: Comonad[F], am: Arbitrary[F[Int]],
                   af: Arbitrary[F[Int] => Int], e: Equal[F[Int]]) = new Properties("comonad") {
      include(cobind.laws[F])
      property("cobind left identity") = cobindLeftIdentity[F, Int]
      property("cobind right identity") = cobindRightIdentity[F, Int, Int]
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

    def laws[F[_]](implicit fa: Arbitrary[F[Int]], F: Traverse[F], EF: Equal[F[Int]]) =
      new Properties("traverse") {
        property("identity traverse") = identityTraverse[F, Int, Int]

        import std.list._, std.option._, std.stream._

        property("purity.option") = purity[F, Option, Int]
        property("purity.stream") = purity[F, Stream, Int]

        property("sequential fusion") = sequentialFusion[F, Option, List, Int, Int, Int]
      }
  }

  object bitraverse {
    def laws[F[_, _]](implicit fa: Arbitrary[F[Int,Int]], F: Bitraverse[F], EF: Equal[F[Int, Int]]) =
      new Properties("bitraverse") {
        private implicit val left = F.leftTraverse[Int]
        private implicit val right = F.rightTraverse[Int]
        include(traverse.laws[({type f[a]=F[a, Int]})#f])
        include(traverse.laws[({type f[a]=F[Int, a]})#f])
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

  object isEmpty {
    def emptyIsEmpty[F[_], X](implicit f: IsEmpty[F]):Prop =
      f.isEmptyLaw.emptyIsEmpty[X]

    def emptyPlusIdentity[F[_], X](implicit f: IsEmpty[F], afx: Arbitrary[F[X]]) =
      forAll(f.isEmptyLaw.emptyPlusIdentity[X] _)

    def laws[F[_]](implicit F: IsEmpty[F], afx: Arbitrary[F[Int]], ef: Equal[F[Int]]) = new Properties("isEmpty") {
      include(plusEmpty.laws[F])
      property("empty is empty") = emptyIsEmpty[F, Int]
      property("empty plus identity") =  emptyPlusIdentity[F, Int]
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

  object contravariant {
    def identity[F[_], X](implicit F: Contravariant[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) =
      forAll(F.contravariantLaw.identity[X] _)

    def composite[F[_], X, Y, Z](implicit F: Contravariant[F], af: Arbitrary[F[Z]], axy: Arbitrary[(X => Y)],
                                   ayz: Arbitrary[(Y => Z)], ef: Equal[F[X]]) =
      forAll(F.contravariantLaw.composite[Z, Y, X] _)

    def laws[F[_]](implicit F: Contravariant[F], af: Arbitrary[F[Int]], axy: Arbitrary[(Int => Int)],
                   ef: Equal[F[Int]]) = new Properties("contravariant") {
      include(invariantFunctor.laws[F])
      property("identity") = identity[F, Int]
      property("composite") = composite[F, Int, Int, Int]
    }
  }

  object compose {
    def associative[=>:[_, _], A, B, C, D](implicit ab: Arbitrary[A =>: B], bc: Arbitrary[B =>: C],
                                           cd: Arbitrary[C =>: D], C: Compose[=>:], E: Equal[A =>: D]) =
      forAll(C.composeLaw.associative[A, B, C, D] _)

    def laws[=>:[_, _]](implicit C: Category[=>:], AB: Arbitrary[Int =>: Int], E: Equal[Int =>: Int]) = new Properties("compose") {
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
    def laws[F[_, _]](implicit F: Bifunctor[F], E: Equal[F[Int, Int]], af: Arbitrary[F[Int, Int]],
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

  @deprecated("MetricSpace is deprecated", "7.0.1")
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
