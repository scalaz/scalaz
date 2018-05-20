package scalaz
package std

import java.lang.Double.doubleToRawLongBits

import typeclass.EqClass
import utils._

trait DoubleInstances {
  implicit val doubleDebug: Debug[Double] = toStringDebug[Double]
  implicit val doubleEq: Eq[Double] =
    instanceOf[EqClass[Double]]((a, b) => doubleToRawLongBits(a) == doubleToRawLongBits(b))
}
