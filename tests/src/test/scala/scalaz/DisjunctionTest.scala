package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object DisjunctionTest extends SpecLite {

  checkAll(order.laws[Int \/ Int])
  checkAll(monoid.laws[Int \/ Int])
  checkAll(monad.laws[({type λ[α] = Int \/ α})#λ])
  checkAll(plus.laws[({type λ[α] = Int \/ α})#λ])
  checkAll(traverse.laws[({type λ[α] = Int \/ α})#λ])
  checkAll(bitraverse.laws[\/])

}
