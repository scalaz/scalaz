package scalaz
package std

import utils._

trait ByteInstances {
  implicit val byteDebug: Debug[Byte] = toStringDebug[Byte]
  implicit val byteEq: Eq[Byte]       = universalEq[Byte]

  /* https://github.com/scalaz/scalaz/pull/1792
  implicit val byteOrd: Ord[Byte] = instanceOf(new OrdClass[Byte] {
    def comp(a: Byte, b: Byte) = java.lang.Byte.compare(a, b) match {
      case 0 => EQ
      case x if x < 0 => LT
      case _ => GT
    }
  })*/
}
