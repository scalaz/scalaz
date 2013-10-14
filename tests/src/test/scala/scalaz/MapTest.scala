package scalaz

import org.scalacheck.Arbitrary, Arbitrary.arbitrary
import scalaz.scalacheck.ScalazProperties._
import std.AllInstances._
import org.scalacheck.Prop.forAll


class MapTest extends SpecLite {
  checkAll(order.laws[Map[Int, String]])
  checkAll(monoid.laws[Map[Int, String]])
  checkAll(traverse.laws[({type λ[α]=Map[Int, α]})#λ])
  checkAll(equal.laws[Map[Int, String]])
  checkAll(equal.laws[Map[NotNatural, String]])

  case class NotNatural(id: Int)
  implicit def NotNaturalArbitrary: Arbitrary[NotNatural] =
    Arbitrary(arbitrary[Int] map (NotNatural.apply))

  implicit def NotNaturalOrder: Order[NotNatural] =
    Order.orderBy[NotNatural, Int](_.id)

  implicit def NotNaturalEqual: Equal[NotNatural] = new Equal[NotNatural] {
    def equal(a1: NotNatural, a2: NotNatural): Boolean = a1.id == a2.id
  }
}
