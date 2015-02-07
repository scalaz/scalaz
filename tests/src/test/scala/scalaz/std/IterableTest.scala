package scalaz
package std

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object IterableTest extends SpecLite {
  import std.iterable._
  import std.anyVal._

  checkAll(order.laws[Iterable[Boolean]].withProp("benchmark", order.scalaOrdering[Iterable[Boolean]]))

  checkAll(FoldableTests.anyAndAllLazy[Iterable])
}
