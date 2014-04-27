package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Const._
import org.scalacheck.Prop.forAll

object ConstTest extends SpecLite {
  checkAll("Const", equal.laws[Const[Int, String]])
  checkAll("Const", functor.laws[({type λ[α]  = Const[Int, α]})#λ])

  checkAll("Const List"  , applicative.laws[({type λ[α]  = Const[List[Int], α]})#λ])
  checkAll("Const Option", applicative.laws[({type λ[α]  = Const[Option[Int], α]})#λ])


  "const functions" in {
    "const" ! forAll { (x: Int, y: Function0[String]) =>
      const(x)(y) == x
    }
  }

  object instances {
    def functor[C] = Functor[({type λ[α] = Const[C, α]})#λ]
    def apply[C: Monoid] = Apply[({type λ[α] = Const[C, α]})#λ]
    def applicative[C: Monoid] = Applicative[({type λ[α] = Const[C, α]})#λ]
  }
}
