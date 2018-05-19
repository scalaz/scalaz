package scalaz
package std

import utils._

trait BooleanInstances {
  implicit val booleanDebug: Debug[Boolean] = toStringDebug[Boolean]
  implicit val booleanEq: Eq[Boolean]       = universalEq[Boolean]

  /* https://github.com/scalaz/scalaz/pull/1792
  implicit val booleanOrd: Ord[Boolean] = instanceOf(new OrdClass[Boolean] {
    def comp(a: Boolean, b: Boolean) = (a, b) match {
      case (true, false) => GT
      case (false, true) => LT
      case _ => EQ
    }
  })*/
}
