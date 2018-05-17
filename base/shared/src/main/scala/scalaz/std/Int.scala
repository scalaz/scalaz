package scalaz
package std

import utils._

trait IntInstances {
  implicit val intDebug: Debug[Int] = toStringDebug[Int]
  implicit val intEq: Eq[Int]       = universalEq[Int]
}
