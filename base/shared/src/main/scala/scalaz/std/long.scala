package scalaz
package std

import algebra.OrdClass
import utils._

trait LongInstances {
  implicit val longDebug: Debug[Long] = toStringDebug[Long]
  implicit val longEq: Eq[Long]       = universalEqAnyVal[Long]

  implicit val longOrd: Ord[Long] = instanceOf(
    ((a: Long,
      b: Long) =>
       java.lang.Long.compare(a, b) match {
         case 0          => EQ
         case x if x < 0 => LT
         case _          => GT
       }): OrdClass[Long]
  )
}
