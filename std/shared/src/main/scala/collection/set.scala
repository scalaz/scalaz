package scalaz
package std

import scala.collection.immutable.Set

import scalaz.core.EqClass

trait SetInstances {
  implicit def setEq[A: Eq]: Eq[Set[A]] =
    instanceOf(new EqClass[Set[A]] {
      def equal(first: Set[A], second: Set[A]): Boolean = (first.toStream.corresponds(second.toStream)(Eq[A].equal))
    })
}
