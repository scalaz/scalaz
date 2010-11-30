package scalaz

import concurrent.Promise
import org.scalacheck.{Arbitrary, Prop}
import org.specs.{Specification, ScalaCheck, Sugar}
import java.math.BigInteger
import scalacheck.{ScalazProperties, ScalazArbitrary, ScalaCheckBinding}

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
    checkEqualLaws[(A)]
    checkEqualLaws[(A, B)]
    checkEqualLaws[(A, B, C)]
    checkEqualLaws[(A, B, C, D)]
    checkEqualLaws[(A, B, C, D, E)]
    checkEqualLaws[(A, B, C, D, E, F)]
    checkEqualLaws[(A, B, C, D, E, F, G)]
    checkEqualLaws[() => A]
    checkEqualLaws[Option[A]]
    checkEqualLaws[FirstOption[A]]
    checkEqualLaws[LastOption[A]]
    checkEqualLaws[Either[A, B]]
    checkEqualLaws[Either.LeftProjection[A, X]]
    checkEqualLaws[Either.RightProjection[X, A]]
    checkEqualLaws[Validation[E, A]]
    checkEqualLaws[FailProjection[E, A]]
    // todo Arbitrary for Tree producing large (infinite?) trees.
    //    checkEqualLaws[Tree[A]]
    //    checkEqualLaws[TreeLoc[A]]

    // todo add Arbitrary instance
    //checkEqualLaws[Promise[A]]
    checkEqualLaws[List[A]]
    checkEqualLaws[Stream[A]]
    checkEqualLaws[ArraySeq[A]]

    import geo._
    checkEqualLaws[Azimuth]
    checkEqualLaws[Bearing]
    checkEqualLaws[Coord]
    checkEqualLaws[ElevatedCurve]
    checkEqualLaws[Elevation]
    checkEqualLaws[Ellipsoid]
    checkEqualLaws[GeodeticCurve]
    checkEqualLaws[Latitude]
    checkEqualLaws[Longitude]
    checkEqualLaws[Position]
    checkEqualLaws[Vector]

    // todo add Arbitrary instances
    // java collections
    //checkEqualLaws[java.lang.Iterable[A]]
    //checkEqualLaws[java.util.Map.Entry[K, V]]
    //checkEqualLaws[java.util.Map[K, V]]
    checkEqualLaws[java.util.concurrent.Callable[A]]
    checkEqualLaws[Zipper[A]]
  }
  
  "collection equality" in {
    def equalityConsistent[C](c1: C, c2: C)(implicit eq: Equal[C]): Boolean = (c1 == c2) must be_==(c1 === c2)
    equalityConsistent(Set(1, 2, 3), Set(3, 2, 1))
    equalityConsistent(Map(1 -> 2, 2 -> 3, 3 -> 4), Map(3 -> 4, 1 -> 2, 2 -> 3))

    class A(val a: Int)
    implicit val aEq: Equal[A] = equalBy(_.a)
    (Set(new A(1), new A(1), new A(2)) === Set(new A(2), new A(1))) must beTrue
  }

  def checkEqualLaws[A: Equal : Manifest : Arbitrary]: Unit = {
    val typeName = manifest[A].toString
    typeName in {
      import ScalazProperties.Equal._
      commutativity[A] must pass
      identity[A] must pass
      checkEqualsNotBasedOnObjectIdentity[A]
    }
  }

  def checkEqualsNotBasedOnObjectIdentity[A: Equal : Manifest : Arbitrary]: Unit = {
    Prop.forAll((as: Duplicate[A]) => as.pair match {
      case (a1, a2) =>  a1 ≟ a2
    }).label("checkEqualsNotBasedOnObjectIdentity") must pass
  }
}