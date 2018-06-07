package scalaz
package std

import scala.collection.immutable.Set

import core.EqAnyRef

trait SetInstances {
  implicit def setEq[A: Eq]: Eq[Set[A]] =
    instanceOf(((a, b) => (a.toStream corresponds b.toStream)(Eq[A].equal)): EqAnyRef[Set[A]])
}
