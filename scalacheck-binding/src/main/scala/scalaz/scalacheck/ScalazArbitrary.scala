package scalaz
package scalacheck

import java.math.BigInteger
import org.scalacheck.{Pretty, Gen, Arbitrary}
import java.io._

/**
 * Instances of {@link scalacheck.Arbitrary} for many types in Scalaz.
 */
object ScalazArbitrary {
  import Scalaz._
  import Arbitrary._
  import Gen._
  import ScalaCheckBinding._

  // todo report and/or work around compilation error: "scalaz is not an enclosing class"
  // implicit def ShowPretty[A: Show](a: A): Pretty = Pretty { _ => a.show }

  private def arb[A: Arbitrary]: Arbitrary[A] = implicitly[Arbitrary[A]]

  implicit def IdentityArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Identity[A]] =
    a ∘ ((x: A) => IdentityTo(x))

  implicit def UnitArbitrary: Arbitrary[Unit] = Arbitrary(value(()))

  implicit def AlphaArbitrary: Arbitrary[Alpha] = Arbitrary(oneOf(alphas))

  implicit def BooleanConjunctionArbitrary: Arbitrary[BooleanConjunction] = arb[Boolean] ∘ ((_: Boolean).|∧|)

  implicit def arbBigInt: Arbitrary[BigInt] = arb[Int].<**>(arb[Int])(_ * _)

  implicit def arbBigInteger: Arbitrary[BigInteger] = arb[BigInt] ∘ (_.bigInteger)

  implicit def BigIntegerMultiplicationArbitrary: Arbitrary[BigIntegerMultiplication] = arb[BigInteger] ∘ ((_: BigInteger).∏)

  implicit def BigIntMultiplicationArbitrary: Arbitrary[BigIntMultiplication] = arb[BigInt] ∘ ((_: BigInt).∏)

  implicit def ByteMultiplicationArbitrary: Arbitrary[ByteMultiplication] = arb[Byte] ∘ ((_: Byte).∏)

  implicit def CharMultiplicationArbitrary: Arbitrary[CharMultiplication] = arb[Char] ∘ ((_: Char).∏)

  implicit def ShortMultiplicationArbitrary: Arbitrary[ShortMultiplication] = arb[Short] ∘ ((_: Short).∏)

  implicit def IntMultiplicationArbitrary: Arbitrary[IntMultiplication] = arb[Int] ∘ ((_: Int).∏)

  implicit def LongMultiplicationArbitrary: Arbitrary[LongMultiplication] = arb[Long] ∘ ((_: Long).∏)

  implicit def DigitArbitrary: Arbitrary[Digit] = Arbitrary(oneOf(digits))

  implicit def DListArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[DList[A]] = arb[List[A]] ∘ (as => dlist(_ => as))

  implicit def NonEmptyListArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[NonEmptyList[A]] = arb[A].<**>(arb[List[A]])(nel _)

  implicit def OrderingArbitrary: Arbitrary[Ordering] = Arbitrary(oneOf(LT, EQ, GT))

  implicit def TreeArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Tree[A]] = Arbitrary {
    def tree(n: Int): Gen[Tree[A]] = n match {
      case 0 => arbitrary[A] ∘ (leaf _)
      case n => {
        val nextSize = n.abs / 2
        arbitrary[A].<**>(resize(n, containerOf[Stream, Tree[A]](Arbitrary(tree(nextSize)).arbitrary)))(node _)
      }
    }
    Gen.sized(tree _)
  }

  implicit def TreeLocArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[TreeLoc[A]] = arb[Tree[A]] ∘ ((t: Tree[A]) => t.loc)

  implicit def ValidationArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Validation[A, B]] = arb[Either[A, B]] ∘ (validation _)

  implicit def FailProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[FailProjection[A, B]] = arb[Validation[A, B]] ∘ (_.fail)

  implicit def ZipStreamArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[ZipStream[A]] = arb[Stream[A]] ∘ ((s: Stream[A]) => s.ʐ)

  implicit def Tuple1Arbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Tuple1[A]] = arb[A] ∘ ((x: A) => Tuple1(x))

  implicit def Function0Arbitrary[A](implicit a: Arbitrary[A]): Arbitrary[() => A] = arb[A] ∘ ((x: A) => () => x)

  implicit def FirstOptionArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[FirstOption[A]] = arb[Option[A]] ∘ ((x: Option[A]) => x.fst)

  implicit def LastOptionArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[LastOption[A]] = arb[Option[A]] ∘ ((x: Option[A]) => x.lst)

  implicit def EitherLeftProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Either.LeftProjection[A, B]] = arb[Either[A, B]] ∘ ((x: Either[A, B]) => x.left)

  implicit def EitherRightProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Either.RightProjection[A, B]] = arb[Either[A, B]] ∘ ((x: Either[A, B]) => x.right)

  implicit def ArraySeqArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[ArraySeq[A]] = arb[List[A]] ∘ ((x: List[A]) => ArraySeq(x: _*))

  import FingerTree._
  
  implicit def FingerArbitrary[V, A](implicit a: Arbitrary[A], measure: Reducer[A, V]): Arbitrary[Finger[V, A]] = Arbitrary(oneOf(
    arbitrary[A] ∘ (one(_): Finger[V, A]),
    (arbitrary[A] ⊛ arbitrary[A])(two(_, _): Finger[V, A]),
    (arbitrary[A] ⊛ arbitrary[A] ⊛ arbitrary[A])(three(_, _, _): Finger[V, A]),
    (arbitrary[A] ⊛ arbitrary[A] ⊛ arbitrary[A] ⊛ arbitrary[A])(four(_, _, _, _): Finger[V, A])
  ))

  implicit def NodeArbitrary[V, A](implicit a: Arbitrary[A], measure: Reducer[A, V]): Arbitrary[Node[V, A]] = Arbitrary(oneOf(
    (arbitrary[A] ⊛ arbitrary[A])(node2[V, A] _),
    (arbitrary[A] ⊛ arbitrary[A] ⊛ arbitrary[A])(node3[V, A] _)
  ))

  implicit def FingerTreeArbitrary[V, A](implicit a: Arbitrary[A], measure: Reducer[A, V]): Arbitrary[FingerTree[V, A]] = Arbitrary {
    def fingerTree[A](n: Int)(implicit a1: Arbitrary[A], measure1: Reducer[A, V]): Gen[FingerTree[V, A]] = n match {
      case 0 => empty[V, A]
      case 1 => arbitrary[A] ∘ (single[V, A] _)
      case n => {
        val nextSize = n.abs / 2
        (arbitrary[Finger[V, A]] ⊛ fingerTree[Node[V, A]](nextSize) ⊛ arbitrary[Finger[V, A]])(deep[V, A](_, _, _))
      }
    }
    Gen.sized(fingerTree[A] _)
  }

  import java.util.concurrent.Callable

  implicit def CallableArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Callable[A]] = arb[A] ∘ ((x: A) => x.η[Callable])

  import concurrent.Promise

  implicit def PromiseArbitrary[A](implicit a: Arbitrary[A], s: concurrent.Strategy[Unit]): Arbitrary[Promise[A]] = arb[A] ∘ ((x: A) => promise(x))

  implicit def ZipperArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Zipper[A]] = arb[Stream[A]].<***>(arb[A], arb[Stream[A]])(zipper[A](_, _, _))

  import geo._
  import Geo._
  implicit def AzimuthArbitrary: Arbitrary[Azimuth] = arbDouble ∘ (azimuth _)

  implicit def BearingArbitrary: Arbitrary[Bearing] = arbDouble ∘ (bearing _)

  implicit def CoordArbitrary: Arbitrary[Coord] = arb[Latitude].<**>(arb[Longitude])(coord _)

  implicit def ElevatedCurveArbitrary: Arbitrary[ElevatedCurve] = arb[GeodeticCurve].<**>(arb[Elevation])(elevatedCurve _)

  implicit def ElevationArbitrary: Arbitrary[Elevation] = arbDouble ∘ (elevation _)

  implicit def EllipsoidArbitrary: Arbitrary[Ellipsoid] = arbDouble.<****>(arbDouble, arbDouble, arbDouble)(ellipsoid _)

  implicit def GeodeticCurveArbitrary: Arbitrary[GeodeticCurve] = arbDouble.<***>(arb[Azimuth], arb[Azimuth])(curve _)

  implicit def LatitudeArbitrary: Arbitrary[Latitude] = arbDouble ∘ (latitude _)

  implicit def LongitudeArbitrary: Arbitrary[Longitude] = arbDouble ∘ (longitude _)

  implicit def PositionArbitrary: Arbitrary[Position] = arb[Coord].<**>(arb[Elevation])(position _)

  implicit def VectorArbitrary: Arbitrary[Vector] = arb[Coord].<**>(arb[Bearing])(vector _)

  // workaround bug in Scalacheck 1.8-SNAPSHOT.
  private def arbDouble: Arbitrary[Double] = Arbitrary { Gen.oneOf(posNum[Double], negNum[Double])}

  trait Duplicate[A] {
    def pair: (A, A)
  }

  /**
   * An Arbitrary that calls the underlying generator twice with each random value, returning
   * a pair of identically created values. This is useful to test that equality is not based on
   * object identity.
   */
  implicit def DuplicateArbitrary[A](implicit aa: Arbitrary[A]): Arbitrary[Duplicate[A]] = Arbitrary {
    Gen {
      (params: Gen.Params) => {
        val Params(_, rng) = params
        // Because java.util.Random is side-effectful, it cannot be shared.
        // Furthermore, the only way to copy one is through serialization.
        def cloneParams = params.copy(rng = Serialization.clone(rng))
        def a: Option[A] = aa.arbitrary(cloneParams)
        (a <|*|> a) ∘ (as => new Duplicate[A] {
          def pair = as
        })
      }
    }
  }
}

private[scalacheck] object Serialization {
  def clone[A <: Serializable](obj: A): A = deserialize(serialize(obj))

  def serialize[A <: Serializable](obj: A, outputStream: OutputStream): Unit = {
    var out = new ObjectOutputStream(outputStream)
    try {
      out.writeObject(obj)
    } finally {
      out.close
    }
  }

  def deserialize[A <: Serializable](objectData: Array[Byte]): A = deserialize(new ByteArrayInputStream(objectData))

  def serialize(obj: Serializable): Array[Byte] = {
    var baos = new ByteArrayOutputStream(512)
    serialize(obj, baos)
    baos.toByteArray
  }

  def deserialize[A <: Serializable](inputStream: InputStream): A = {
    var in = new ObjectInputStream(inputStream)
    try {
      return in.readObject.asInstanceOf[A]
    }
    finally {
      in.close
    }
  }
}


