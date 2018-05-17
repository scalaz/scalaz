package scalaz
package std

import utils._

trait ByteInstances {
  implicit val byteDebug: Debug[Byte] = toStringDebug[Byte]
  implicit val byteEq: Eq[Byte]       = universalEq[Byte]
}
