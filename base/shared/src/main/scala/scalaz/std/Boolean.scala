package scalaz
package std

import utils._

trait BooleanInstances {
  implicit val booleanDebug: Debug[Boolean] = toStringDebug[Boolean]
  implicit val booleanEq: Eq[Boolean]       = universalEq[Boolean]
}
