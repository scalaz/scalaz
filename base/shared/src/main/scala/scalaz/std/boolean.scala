package scalaz
package std

import algebra.OrdClass
import utils._

trait BooleanInstances {
  implicit val booleanDebug: Debug[Boolean] = toStringDebug[Boolean]
  implicit val booleanEq: Eq[Boolean]       = universalEqAnyVal[Boolean]

  implicit val booleanOrd: Ord[Boolean] = instanceOf({
    case (true, false) => GT
    case (false, true) => LT
    case _             => EQ
  }: OrdClass[Boolean])
}
