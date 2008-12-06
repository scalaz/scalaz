package scalaz

import fjs.test.Arbitrary
import fjs.test.Arbitrary.arb
import fj.test.Gen.elements

object ArbitraryDigit {
  implicit def arbDigit(): Arbitrary[Digit] = arb(elements[Digit](Digit.digits.toArray: _*))
}
