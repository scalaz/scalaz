package scalaz

import org.scalacheck._
import org.scalacheck.Prop.forAll

import std.anyVal._
import std.list._
import std.string._
import std.tuple._

object ContravariantCoyonedaGens {
  val CtCoOrder = ContravariantCoyoneda.on[Order]
  type CtCoOrder[A] = ContravariantCoyoneda[Order, A]

  def cmappedOrderLaws[A: Arbitrary](co: CtCoOrder[A]) = {
    import co.unlift
    scalaz.scalacheck.ScalazProperties.order.laws[A]
  }

  def aToString[A] = CtCoOrder((_:A).toString)

  val evensFirst = CtCoOrder{x:Int =>
    if (x % 2 == 0) (0, x)
    else (1, x)
  }

  val negated = CtCoOrder{x:Int => -x}

  val probablePrime = CtCoOrder{x:Int =>
    ((x - 1 : BigInt) isProbablePrime 5, x)
  }

  val intOrders: Gen[CtCoOrder[Int]] =
    Gen.oneOf(aToString[Int], evensFirst, negated, probablePrime)
}

object ContravariantCoyonedaTest extends SpecLite {
  import ContravariantCoyonedaGens._

  implicit val arbIntOrders = Arbitrary(intOrders)

  checkAll(cmappedOrderLaws(aToString[Int]))
  checkAll(cmappedOrderLaws(evensFirst))
  checkAll(cmappedOrderLaws(negated))
  checkAll(cmappedOrderLaws(probablePrime))

  "contravariant identity law" ! forAll {(xs: List[Int]) =>
    val co = CtCoOrder(conforms[Int])
    xs.sorted(co.unlift.toScalaOrdering) must_=== xs.sorted
  }

  "Schwartzian-transformed sort equals normal sort" ! forAll{
    (xs: List[Int], o: CtCoOrder[Int]) =>
    xs.map(o.schwartzianPre)
      .sorted(o.schwartzianOrder.toScalaOrdering)
      .map(o.schwartzianPost) must_=== xs.sorted(o.unlift.toScalaOrdering)
  }
}
