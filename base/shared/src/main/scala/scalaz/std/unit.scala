package scalaz
package std

import utils._

trait UnitInstances {
  implicit val unitDebug: Debug[Unit] = toStringDebug[Unit]
  implicit val unitEq: Eq[Unit]       = singletonEq[Unit]
}
