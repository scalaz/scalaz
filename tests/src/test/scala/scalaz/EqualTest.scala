package scalaz

import concurrent.Promise
import org.scalacheck.Arbitrary
import scalacheck.{ScalazArbitrary, ScalaCheckBinding}
import org.specs.{Specification, ScalaCheck, Sugar}
import java.math.BigInteger

class EqualTest extends Specification with Sugar with ScalaCheck {
  import Scalaz._
  import ScalaCheckBinding._
  import ScalazArbitrary._

  "equal laws" should {
    type A = String
    type B = String
    type C = String
    type D = String
    type E = String
    type F = String
    type G = String
    type K = String
    type V = String
    type X = String

    // AnyVal
    checkEqualLaws[Unit]
    checkEqualLaws[Boolean]
    checkEqualLaws[Char]
    checkEqualLaws[Short]
    checkEqualLaws[Int]
    checkEqualLaws[Long]
    checkEqualLaws[Float]

    checkEqualLaws[Digit]
    checkEqualLaws[Ordering]
    checkEqualLaws[String]
    checkEqualLaws[IntMultiplication]
    checkEqualLaws[BooleanConjunction]
    checkEqualLaws[CharMultiplication]
    checkEqualLaws[ByteMultiplication]
    checkEqualLaws[LongMultiplication]
    checkEqualLaws[ShortMultiplication]
    checkEqualLaws[BigInteger]
    checkEqualLaws[BigIntegerMultiplication]
    checkEqualLaws[BigInt]
    checkEqualLaws[BigIntMultiplication]
    // todo Arbitrary instance
    // checkEqualLaws[xml.NodeSeq]
    checkEqualLaws[NonEmptyList[A]]
    checkEqualLaws[ZipStream[A]]
    checkEqualLaws[Tuple1[A]]
    checkEqualLaws[(A, B)]
    checkEqualLaws[(A, B, C)]
    checkEqualLaws[(A, B, C, D)]
    checkEqualLaws[(A, B, C, D, E)]
    checkEqualLaws[(A, B, C, D, E, F)]
    checkEqualLaws[(A, B, C, D, E, F, G)]
    checkEqualLaws[Function0[A]]
    checkEqualLaws[Option[A]]
    checkEqualLaws[FirstOption[A]]
    checkEqualLaws[LastOption[A]]
    checkEqualLaws[Either[A, B]]
    checkEqualLaws[Either.LeftProjection[A, X]]
    checkEqualLaws[Either.RightProjection[X, A]]
    checkEqualLaws[Validation[E, A]]
    // todo Arbitrary for Tree producing large (infinite?) trees.
    //    checkEqualLaws[Tree[A]]
    //    checkEqualLaws[TreeLoc[A]]

    // todo add Arbitrary instance
    //checkEqualLaws[Promise[A]]
    checkEqualLaws[List[A]]
    checkEqualLaws[Stream[A]]
    checkEqualLaws[GArray[A]]

    // todo add Arbitrary instances
    //    import geo._
    //    checkEqualLaws[Azimuth]
    //    checkEqualLaws[Bearing]
    //    checkEqualLaws[Coord]
    //    checkEqualLaws[ElevatedCurve]
    //    checkEqualLaws[Elevation]
    //    checkEqualLaws[Ellipsoid]
    //    checkEqualLaws[GeodeticCurve]
    //    checkEqualLaws[Latitude]
    //    checkEqualLaws[Longitude]
    //    checkEqualLaws[Position]
    //    checkEqualLaws[Vector]

    // todo add Arbitrary instances
    // java collections
    //checkEqualLaws[java.lang.Iterable[A]]
    //checkEqualLaws[java.util.Map.Entry[K, V]]
    //checkEqualLaws[java.util.Map[K, V]]
  }

  def checkEqualLaws[A: Equal : Manifest : Arbitrary] = {
    val typeName = manifest[A].toString
    import EqualLaw._
    (typeName ⊹ ": commutativityLaw") verifies commutativityLaw[A]
    (typeName ⊹ ": identityLaw") verifies identityLaw[A]
    checkEqualsNotBasedOnObjectIdentity[A]
  }

  def checkEqualsNotBasedOnObjectIdentity[A: Equal : Manifest : Arbitrary] = {
    val typeName = manifest[A].toString
    implicit val da: Arbitrary[(A, A)] = DuplicateArbitrary[A]
    import Arbitrary.{arbTuple2 => _, _}
    (typeName ⊹ ": checkEqualsNotBasedOnObjectIdentity") verifies {
      (t: Tuple1[Tuple2[A, A]]) => {
        val Tuple1((a1, a2)) = t
        a1 ≟ a2
      }
    }
  }

  // todo report scala bug: forward reference extends over definition of value da
/*
  def checkEqualLaws[A: Equal : Manifest : Arbitrary] = {
    val typeName = manifest[A].toString
    import EqualLaw._
    (typeName ⊹ ": commutativityLaw") verifies commutativityLaw[A]
    (typeName ⊹ ": identityLaw") verifies identityLaw[A]
    implicit val da: Arbitrary[(A, A)] = DuplicateArbitrary[A]
    import Arbitrary.{arbTuple2 => _}
    (typeName ⊹ ": identityLaw2") verifies {
      (t: Tuple1[Tuple2[A, A]]) => {
        t._1._1 ≟ t._1._2
      }
    }
  }
*/
}