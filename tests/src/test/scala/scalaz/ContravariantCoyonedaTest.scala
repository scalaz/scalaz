package scalaz

import org.scalacheck._
import org.scalacheck.Prop.forAll

import std.anyVal._
import std.list._
import std.string._
import std.tuple._

object ContravariantCoyonedaGens {
  val CtCoOrder = ContravariantCoyoneda.by[Order]
  type CtCoOrder[A] = ContravariantCoyoneda[Order, A]

  final class Schwartzian[F[_], A, FA <: ContravariantCoyoneda[F, A]](val self: FA) /*extends AnyVal*/ {
    import self._
    @inline def schwartzianPre: A => (I, A) = a => (k(a), a)
    @inline def schwartzianPost: ((I, A)) => A = _._2
    @inline def schwartzianOrder(implicit F: Contravariant[F])
        : F[(I, A)] = F.contramap(fi)(_._1)
  }

  @inline implicit def Schwartzian[F[_], A](co: ContravariantCoyoneda[F, A])
      : Schwartzian[F, A, co.type] = new Schwartzian[F, A, co.type](co)

  def cmappedOrderLaws[A: Arbitrary](co: CtCoOrder[A]) = {
    implicit val ran = co.run
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
    Gen.oneOf[CtCoOrder[Int]](
      aToString[Int], evensFirst, negated, probablePrime)
}

object ContravariantCoyonedaTest extends SpecLite {
  import ContravariantCoyonedaGens._

  implicit val arbIntOrders = Arbitrary(intOrders)

  checkAll(cmappedOrderLaws(aToString[Int]))
  checkAll(cmappedOrderLaws(evensFirst))
  checkAll(cmappedOrderLaws(negated))
  checkAll(cmappedOrderLaws(probablePrime))

  "contravariant identity law" ! forAll {(xs: List[Int]) =>
    val co = CtCoOrder(identity[Int])
    xs.sorted(co.run.toScalaOrdering) must_=== xs.sorted
  }

  "Schwartzian-transformed sort equals normal sort" ! forAll{
    (xs: List[Int], o: CtCoOrder[Int]) =>
    xs.map(o.schwartzianPre)
      .sorted(o.schwartzianOrder.toScalaOrdering)
      .map(o.schwartzianPost) must_=== xs.sorted(o.run.toScalaOrdering)
  }
}
