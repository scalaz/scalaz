package scalaz
package std

import utils._

trait LongInstances {
  implicit val longDebug: Debug[Long] = toStringDebug[Long]
  implicit val longEq: Eq[Long]       = universalEq[Long]
}
