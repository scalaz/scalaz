package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Const._
import org.scalacheck.Prop.forAll

object ConstTest extends SpecLite {
  checkAll("Const", order.laws[Const[Int, String]])

  checkAll("Const List"  , applicative.laws[({type λ[α]  = Const[List[Int], α]})#λ])
  checkAll("Const Option", applicative.laws[({type λ[α]  = Const[Option[Int], α]})#λ])

  checkAll(contravariant.laws[({type λ[α] = Const[Int, α]})#λ])

  "const functions" in {
    "const" ! forAll { (x: Int, y: Function0[String]) =>
      const(x)(y) == x
    }
  }

  object instances {
    def functor[C] = Functor[({type λ[α] = Const[C, α]})#λ]
    def functorMax[C: Monoid] = Functor[({type λ[α] = Const[C, α]})#λ]
    def apply[C: Semigroup] = Apply[({type λ[α] = Const[C, α]})#λ]
    def applicative[C: Monoid] = Applicative[({type λ[α] = Const[C, α]})#λ]
    def equal[C: Equal, A] = Equal[Const[C, A]]
    def equalMax[C: Order, A] = Equal[Const[C, A]]
    def order[C: Order, A] = Order[Const[C, A]]
  }
}
