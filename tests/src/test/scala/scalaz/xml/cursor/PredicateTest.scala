package scalaz
package xml
package cursor

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Predicate._
import org.scalacheck.Prop.forAll

object PredicateTest extends SpecLite {
  checkAll(equal.laws[Predicate[Int]])
  checkAll(lens.laws(namePredicateL[Int]))
}
