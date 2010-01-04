package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import org.scalacheck.Arbitrary
import java.math.BigInteger
import scalacheck.{ScalazProperties, ScalazArbitrary, ScalaCheckBinding}

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

  def checkMonoidLaws[A: Monoid : Equal : Manifest : Arbitrary]: Unit = {
    val typeName = manifest[A].toString
    typeName in {
      import ScalazProperties.Semigroup._
      import ScalazProperties.Monoid._
      associative[A] must pass
      identity[A] must pass
    }
  }
}
