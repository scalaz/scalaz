package scalaz
package std

import algebra.OrdClass
import utils._

trait LongInstances {
  implicit val longDebug: Debug[Long] = toStringDebug[Long]
  implicit val longEq: Eq[Long]       = universalEq[Long]

  implicit val longOrd: Ord[Long] = instanceOf(new OrdClass[Long] {
    def comp(a: Long, b: Long) = _root_.java.lang.Long.compare(a, b) match {
      case 0          => EQ
      case x if x < 0 => LT
      case _          => GT
    }
  })
}
