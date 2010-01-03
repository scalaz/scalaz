package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import org.scalacheck.Arbitrary
import java.math.BigInteger
import scalacheck.{ScalazArbitrary, ScalaCheckBinding}

class MonoidTest extends Specification with Sugar with ScalaCheck {
  import Scalaz._
  import ScalaCheckBinding._
  import ScalazArbitrary._

  "function monoid" in {
    val f: (Int) => Int = implicitly[Semigroup[Int => Int]].append(_ + 1, _ * 2)
    f(1) must_== 4
  }

  "monoid laws" should {
    checkMonoidLaws[BooleanConjunction]
    checkMonoidLaws[Boolean]
    checkMonoidLaws[Digit]
    checkMonoidLaws[Int]
    checkMonoidLaws[String]
    checkMonoidLaws[List[Int]]
    checkMonoidLaws[BigInt]
    checkMonoidLaws[BigInteger]
    checkMonoidLaws[BigIntMultiplication]
    checkMonoidLaws[BigIntegerMultiplication]
    checkMonoidLaws[Short]
    checkMonoidLaws[ShortMultiplication]
    checkMonoidLaws[Byte]
    checkMonoidLaws[ByteMultiplication]
    checkMonoidLaws[Long]
    checkMonoidLaws[LongMultiplication]
    // todo more types
  }

  def checkMonoidLaws[A: Monoid : Equal : Manifest : Arbitrary] = {
    val typeName = manifest[A].toString
    import SemigroupLaw.associativeLaw
    (typeName + ": associativeLaw") verifies associativeLaw[A]
    import MonoidLaw.identityLaw
    (typeName + ": identityLaw") verifies identityLaw[A]
  }
}
