package scalaz
package std

import algebra.OrdClass
import utils._

trait ByteInstances {
  implicit val byteDebug: Debug[Byte] = toStringDebug[Byte]
  implicit val byteEq: Eq[Byte]       = universalEqAnyVal[Byte]

  implicit val byteOrd: Ord[Byte] = instanceOf(
    ((a: Byte,
      b: Byte) =>
       java.lang.Byte.compare(a, b) match {
         case 0          => EQ
         case x if x < 0 => LT
         case _          => GT
       }): OrdClass[Byte]
  )
}
