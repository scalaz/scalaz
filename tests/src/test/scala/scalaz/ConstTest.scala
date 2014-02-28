package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Const._
import org.scalacheck.Prop.forAll

object ConstTest extends SpecLite {
  checkAll(functor.laws[({type l[a] = Const[Int, a]})#l])
  checkAll(equal.laws[Const[Int, String]])

  "const functions" in {
    "const" ! forAll { (x: Int, y: String) =>
      const(x)(y) == x
    }
  }
}
