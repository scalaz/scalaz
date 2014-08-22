package scalaz
package std

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop.forAll

object IterableTest extends SpecLite {

  import std.iterable._

  checkAll(order.laws[Iterable[Boolean]].withProp("benchmark", order.scalaOrdering[Iterable[Boolean]]))

  "any is lazy" ! FoldableTests.anyIsLazy[Iterable, Int]
}
