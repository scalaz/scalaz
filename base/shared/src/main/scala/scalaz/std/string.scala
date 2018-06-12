package scalaz
package std

import utils._

import scalaz.algebra.MonoidClass

trait StringInstances {
  implicit val stringDebug: Debug[String] = toStringDebug[String]
  implicit val stringEq: Eq[String]       = universalEq[String]
  implicit val stringMonoid: Monoid[String] = instanceOf(new MonoidClass[String] {
    def mappend(a1: String, a2: => String) = a1 + a2
    def mempty                             = ""
  })
}
