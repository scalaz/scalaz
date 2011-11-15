package scalaz
package scalacheck

import java.math.BigInteger
import org.scalacheck.{Pretty, Gen, Arbitrary}
import java.io._
import collection.mutable.ArraySeq

/**
 * Instances of {@link scalacheck.Arbitrary} for many types in Scalaz.
 */
object ScalazArbitrary {
  import Scalaz._
  import Tags._
  import Arbitrary._
  import Gen._
  import ScalaCheckBinding._
  import syntax.std.booleanV._
  import std.java.util.concurrent.callable._
  import syntax.functor._

  // todo report and/or work around compilation error: "scalaz is not an enclosing class"
  // implicit def ShowPretty[A: Show](a: A): Pretty = Pretty { _ => a.show }

  private def arb[A: Arbitrary]: Arbitrary[A] = implicitly[Arbitrary[A]]

  implicit def ImmutableArrayArbitrary[A](implicit F: Functor[Arbitrary], M: ClassManifest[A], A: Arbitrary[A]) =
    F.map(arbArray[A])(ImmutableArray.fromArray[A](_))

  implicit def IdentityArbitrary[A](implicit fa: Arbitrary[A]): Arbitrary[Identity[A]] = Functor[Arbitrary].map(fa)(a => Need(a))

  implicit def UnitArbitrary: Arbitrary[Unit] = Arbitrary(value(()))

  implicit def AlphaArbitrary: Arbitrary[Alpha] = Arbitrary(oneOf(Alpha.alphas))

  implicit def BooleanConjunctionArbitrary: Arbitrary[Boolean @@ Conjunction] = Functor[Arbitrary].map(arb[Boolean])(_.conjunction)

  implicit def arbBigInt: Arbitrary[BigInt] = Apply[Arbitrary].map2[Int, Int, BigInt](arb[Int], arb[Int])(_ + _)

  implicit def arbBigInteger: Arbitrary[BigInteger] = Functor[Arbitrary].map(arb[BigInt])(_.bigInteger)

  implicit def BigIntegerMultiplicationArbitrary: Arbitrary[BigInteger @@ Multiplication] =
    Tag.subst[BigInteger, Arbitrary, Multiplication](arb[BigInteger])

  implicit def BigIntMultiplicationArbitrary: Arbitrary[BigInt @@ Multiplication] = Tag.subst(arb[BigInt])

  implicit def ByteMultiplicationArbitrary: Arbitrary[Byte @@ Multiplication] = Tag.subst(arb[Byte])

  implicit def CharMultiplicationArbitrary: Arbitrary[Char @@ Multiplication] = Tag.subst(arb[Char])

  implicit def ShortMultiplicationArbitrary: Arbitrary[Short @@ Multiplication] = Tag.subst(arb[Short])

  implicit def IntMultiplicationArbitrary: Arbitrary[Int @@ Multiplication] = Tag.subst(arb[Int])

  implicit def LongMultiplicationArbitrary: Arbitrary[Long @@ Multiplication] = Tag.subst(arb[Long])

  // TODO Is Digit not available anymore?
  // implicit def DigitArbitrary: Arbitrary[Digit] = Arbitrary(oneOf(digits))

  import NonEmptyList._
  implicit def NonEmptyListArbitrary[A: Arbitrary]: Arbitrary[NonEmptyList[A]] = Apply[Arbitrary].map2[A, List[A], NonEmptyList[A]](arb[A], arb[List[A]])(nel(_, _))

  import scalaz.Ordering._
  implicit def OrderingArbitrary: Arbitrary[Ordering] = Arbitrary(oneOf(LT, EQ, GT))

  implicit def TreeArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Tree[A]] = Arbitrary {
  import scalaz.Tree._
    def tree(n: Int): Gen[Tree[A]] = n match {
      case 0 => arbitrary[A] map (leaf(_))
      case n => {
        val nextSize = n.abs / 2
        Apply[Gen].map2(arbitrary[A], resize(n, containerOf[Stream, Tree[A]](Arbitrary(tree(nextSize)).arbitrary)))(node(_, _))
      }
    }
    Gen.sized(tree _)
  }

  implicit def TreeLocArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[TreeLoc[A]] =
    Functor[Arbitrary].map(arb[Tree[A]])((t: Tree[A]) => t.loc)

  import Validation._
  implicit def ValidationArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Validation[A, B]] =
    Functor[Arbitrary].map(arb[Either[A, B]])(fromEither _)

  implicit def FailProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[FailProjection[A, B]] =
    Functor[Arbitrary].map(arb[Validation[A, B]])(_.fail)

//  implicit def ZipStreamArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[ZipStream[A]] = arb[Stream[A]] ∘ ((s: Stream[A]) => s.ʐ)

  implicit def Tuple1Arbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Tuple1[A]] = arb[A] ∘ ((x: A) => Tuple1(x))

  implicit def Function0Arbitrary[A](implicit a: Arbitrary[A]): Arbitrary[() => A] = arb[A] ∘ ((x: A) => () => x)

  implicit def FirstOptionArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Option[A] @@ First] = arb[Option[A]] ∘ ((x: Option[A]) => x.first)

  implicit def LastOptionArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Option[A] @@ Last] = arb[Option[A]] ∘ ((x: Option[A]) => x.last)

  implicit def EitherLeftProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Either.LeftProjection[A, B]] = arb[Either[A, B]] ∘ ((x: Either[A, B]) => x.left)

  implicit def EitherRightProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Either.RightProjection[A, B]] = arb[Either[A, B]] ∘ ((x: Either[A, B]) => x.right)

  implicit def EitherFirstLeftProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Either.LeftProjection[A, B] @@ First] = arb[Either[A, B]] ∘ ((x: Either[A, B]) => Tag(x.left))

  implicit def EitherFirstRightProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Either.RightProjection[A, B] @@ First] = arb[Either[A, B]] ∘ ((x: Either[A, B]) => Tag(x.right))

  implicit def EitherLastLeftProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Either.LeftProjection[A, B] @@ Last] = arb[Either[A, B]] ∘ ((x: Either[A, B]) => Tag(x.left))

  implicit def EitherLastRightProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Either.RightProjection[A, B] @@ Last] = arb[Either[A, B]] ∘ ((x: Either[A, B]) => Tag(x.right))


  implicit def ArraySeqArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[ArraySeq[A]] = arb[List[A]] ∘ ((x: List[A]) => ArraySeq(x: _*))

//  import FingerTree._
//
//  implicit def FingerArbitrary[V, A](implicit a: Arbitrary[A], measure: Reducer[A, V]): Arbitrary[Finger[V, A]] = Arbitrary(oneOf(
//    arbitrary[A] ∘ (one(_): Finger[V, A]),
//    (arbitrary[A] ⊛ arbitrary[A])(two(_, _): Finger[V, A]),
//    (arbitrary[A] ⊛ arbitrary[A] ⊛ arbitrary[A])(three(_, _, _): Finger[V, A]),
//    (arbitrary[A] ⊛ arbitrary[A] ⊛ arbitrary[A] ⊛ arbitrary[A])(four(_, _, _, _): Finger[V, A])
//  ))

//  implicit def NodeArbitrary[V, A](implicit a: Arbitrary[A], measure: Reducer[A, V]): Arbitrary[Node[V, A]] = Arbitrary(oneOf(
//    (arbitrary[A] ⊛ arbitrary[A])((a, b) => node2[V, A](a, b)),
//    (arbitrary[A] ⊛ arbitrary[A] ⊛ arbitrary[A])((a, b, c) => node3[V, A](a, b, c))
//  ))
//
//  implicit def FingerTreeArbitrary[V, A](implicit a: Arbitrary[A], measure: Reducer[A, V]): Arbitrary[FingerTree[V, A]] = Arbitrary {
//    def fingerTree[A](n: Int)(implicit a1: Arbitrary[A], measure1: Reducer[A, V]): Gen[FingerTree[V, A]] = n match {
//      case 0 => empty[V, A]
//      case 1 => arbitrary[A] ∘ (single[V, A](_))
//      case n => {
//        val nextSize = n.abs / 2
//        (arbitrary[Finger[V, A]] ⊛ fingerTree[Node[V, A]](nextSize) ⊛ arbitrary[Finger[V, A]])(deep[V, A](_, _, _))
//      }
//    }
//    Gen.sized(fingerTree[A] _)
//  }
//
//  import FingerTree.ft2ftip
//  implicit def RopeArbitrary[A : Arbitrary : ClassManifest]: Arbitrary[Rope[A]] =
//    FingerTreeArbitrary(ImmutableArrayArbitrary[A], Rope.sizer[A]) ∘ (rope[A](_))

  import java.util.concurrent.Callable

  implicit def CallableArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Callable[A]] = arb[A] ∘ ((x: A) => Pointed[Callable].point(x))

  import concurrent.Promise
  import concurrent.Promise._

  implicit def PromiseArbitrary[A](implicit a: Arbitrary[A], s: concurrent.Strategy): Arbitrary[Promise[A]] = arb[A] ∘ ((x: A) => promise(x))

  import Zipper._
  implicit def ZipperArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Zipper[A]] =
    Apply[Arbitrary].map3[Stream[A], A, Stream[A], Zipper[A]](arb[Stream[A]], arb[A], arb[Stream[A]])(zipper[A](_, _, _))

  // workaround bug in Scalacheck 1.8-SNAPSHOT.
  private def arbDouble: Arbitrary[Double] = Arbitrary { Gen.oneOf(posNum[Double], negNum[Double])}
}
