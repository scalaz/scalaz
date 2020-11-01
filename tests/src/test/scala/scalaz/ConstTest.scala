package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Const._
import org.scalacheck.Prop.forAll

object ConstTest extends SpecLite {
  checkAll("Const", order.laws[Const[Int, String]])

  checkAll("Const List"  , applicative.laws[λ[α => Const[List[Int], α]]])
  checkAll("Const Option", applicative.laws[λ[α => Const[Option[Int], α]]])

  checkAll(traverse.laws[Const[Int, *]])
  checkAll(divisible.laws[Const[Int, *]])

  "const function" ! forAll { (x: Int, y: Function0[String]) =>
    const(x)(y) == x
  }

  object instances {
    def functor[C] = Functor[Const[C, *]]
    def traverse[C] = Traverse[Const[C, *]]
    def functor[C: Semigroup] = Functor[Const[C, *]]
    def functor[C: Monoid] = Functor[Const[C, *]]
    def apply[C: Semigroup] = Apply[Const[C, *]]
    def apply[C: Monoid] = Apply[Const[C, *]]
    def applicative[C: Monoid] = Applicative[Const[C, *]]
    def divide[C: Semigroup] = Divide[Const[C, *]]
    def divisible[C: Monoid] = Divisible[Const[C, *]]
    def equal[C: Equal, A] = Equal[Const[C, A]]
    def equalMax[C: Order, A] = Equal[Const[C, A]]
    def order[C: Order, A] = Order[Const[C, A]]
  }
}
