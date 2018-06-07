package scalaz
package std

import java.lang.Double.doubleToRawLongBits

import algebra.OrdClass
import core.EqClass
import utils._

trait DoubleInstances {
  private[this] final val NegZero: Double = -0.0d
  private[this] final val PosZero: Double = 0.0d

  implicit val doubleDebug: Debug[Double] = toStringDebug[Double]
  implicit val doubleEq: Eq[Double] =
    instanceOf({
      case (NegZero, PosZero) => true
      case (PosZero, NegZero) => true
      case (x, y)             => doubleToRawLongBits(x) == doubleToRawLongBits(y)
    }: EqClass[Double])

  implicit val doubleOrd: Ord[Double] = instanceOf({
    case (a, b) if doubleEq.equal(a, b)                            => EQ
    case (a, b) if doubleToRawLongBits(a) < doubleToRawLongBits(b) => LT
    case _                                                         => GT
  }: OrdClass[Double])
}
