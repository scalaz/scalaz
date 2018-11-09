package scalaz
package std

import scalaz.std.anyVal._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

class NotNatural(val id: Int)

object NotNatural {
  implicit val NotNaturalArbitrary: Arbitrary[NotNatural] =
    Arbitrary(arbitrary[Int] map (new NotNatural(_)))

  implicit val NotNaturalOrder: Order[NotNatural] =
    Order.orderBy[NotNatural, Int](_.id)
}
