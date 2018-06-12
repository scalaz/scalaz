package scalaz
package std

import utils._

import scalaz.algebra.MonoidClass

trait UnitInstances {
  implicit val unitDebug: Debug[Unit] = toStringDebug[Unit]
  implicit val unitEq: Eq[Unit]       = singletonEq[Unit]
  implicit val unitMonoid: Monoid[Unit] = instanceOf(new MonoidClass[Unit] {
    def mappend(a1: Unit, a2: => Unit) = ()
    def mempty                         = ()
  })
}
