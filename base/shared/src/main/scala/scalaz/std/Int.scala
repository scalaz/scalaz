package scalaz
package std

import utils._

trait IntInstances {
  implicit val intDebug: Debug[Int] = toStringDebug[Int]
  implicit val intEq: Eq[Int]       = universalEq[Int]

  /* https://github.com/scalaz/scalaz/pull/1792
  implicit val intOrd: Ord[Int] = instanceOf(new OrdClass[Int] {
    def comp(a: Int, b: Int) = java.lang.Integer.compare(a, b) match {
      case 0 => EQ
      case x if x < 0 => LT
      case _ => GT
    }
  })*/
}
