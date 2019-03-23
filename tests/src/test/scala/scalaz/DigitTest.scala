package scalaz

import Scalaz._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object DigitTest extends SpecLite {
  checkAll(enum.laws[Digit])
  checkAll(monoid.laws[Digit])

  "correctly extract int digits" ! forAll {
    (i: Int) => {
      Digit.digitsFromInt(i).longDigits must_===(scala.math.abs(i.toLong))
    }
  }

  "correctly extract long digits" ! forAll {
    (i: Long) => {
      Digit.digitsFromLong(i).map(_.toInt.toString).suml must_===(i.toString.stripPrefix("-"))
    }
  }

  "correctly extract bigint digits" ! forAll {
    (i: BigInt) => {
      Digit.digitsFromBigInt(i).map(_.toInt.toString).suml must_===(i.toString.stripPrefix("-"))
    }
  }

}
