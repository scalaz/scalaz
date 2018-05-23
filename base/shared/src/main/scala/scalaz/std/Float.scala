package scalaz
package std

import java.lang.Float.floatToRawIntBits
import core.EqClass
import utils._

trait FloatInstances {
  implicit val floatDebug: Debug[Float] = toStringDebug[Float]
  implicit val floatEq: Eq[Float]       = instanceOf[EqClass[Float]]((a, b) => floatToRawIntBits(a) == floatToRawIntBits(b))
}
