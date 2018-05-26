package scalaz
package std

import utils._

trait StringInstances {
  implicit val stringDebug: Debug[String] = toStringDebug[String]
  implicit val stringEq: Eq[String]       = universalEq[String]
}
