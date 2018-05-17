package scalaz
package std

import utils._

trait ShortInstances {
  implicit val shortDebug: Debug[Short] = toStringDebug[Short]
  implicit val shortEq: Eq[Short]       = universalEq[Short]
}
