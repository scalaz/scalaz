package scalaz
package std

import std.AllInstances._
import org.scalacheck.Prop.forAll

object PartialFunctionTest extends SpecLite {
  private type PF[-A, +B] = PartialFunction[A, B]

  private val A = Arrow[PartialFunction]

  private val pfnegonly: PF[Int, Int] = {case a if a < 0 => 0 - a}
  private val pfposonly: PF[Int, Int] = {case a if a >= 0 => 0 - a}

  "arrow equivalence" ! forAll {
    (num: Int, num2: Int) =>
      val nums = (num, num2)
      A.split(A.id[Int], A.id[Int]).isDefinedAt(nums) must_===(true)
      A.split(A.id[Int], A.id[Int])(nums) must_===(nums)
      val definedness = for {
        l <- List(pfnegonly, pfposonly)
        r <- List(pfnegonly, pfposonly)
      } yield (A.split(l, r), A.split(l, r) isDefinedAt (nums))
      (definedness.find(_._2).get._1(nums)
       must_===((0 - num, 0 - num2)))
  }

  "Arrow[PartialFunction]#compose#isDefinedAt" ! forAll{ a: Int =>
    var callF: Int = 0
    var callG: Int = 0
    val f: PartialFunction[Int, Int] = {
      case n if {callF += 1; n % 2 == 0} => n
    }
    val g: PartialFunction[Int, String] = {
      case n if {callG += 1; n % 3 == 0} => n.toString
    }
    val h = A.compose(g, f)
    h.isDefinedAt(a) must_=== (a % 6 == 0)
    callF must_=== 1
    if (a % 2 == 0) {
      callG must_=== 1
    } else {
      callG must_=== 0
    }
  }

  "Arrow[PartialFunction]#compose#applyOrElse" ! forAll{ a: Int =>
    var callF: Int = 0
    var callG: Int = 0
    val f: PartialFunction[Int, Int] = {
      case n if {callF += 1; n % 2 == 0} => n
    }
    val g: PartialFunction[Int, String] = {
      case n if {callG += 1; n % 3 == 0} => n.toString
    }
    val h = A.compose(g, f)
    h.applyOrElse(a, (_: Int) => "default") must_=== (
      if (a % 6 == 0) a.toString
      else "default"
    )
    callF must_=== 1
    if (a % 2 == 0) {
      callG must_=== 1
    } else {
      callG must_=== 0
    }
  }
}
