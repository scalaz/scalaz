package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class PartialFunctionTest extends Spec {
  private type PF[-A, +B] = PartialFunction[A, B]

  private val A = Arrow[PartialFunction]

  private val pfnegonly: PF[Int, Int] = {case a if a < 0 => 0 - a}
  private val pfposonly: PF[Int, Int] = {case a if a >= 0 => 0 - a}

  "arrow equivalence" ! prop {
    (num: Int, num2: Int) =>
      val nums = (num, num2)
      A.split(A.id[Int], A.id[Int]).isDefinedAt(nums) must be_===(true)
      A.split(A.id[Int], A.id[Int])(nums) must be_===(nums)
      val definedness = for {
        l <- List(pfnegonly, pfposonly)
        r <- List(pfnegonly, pfposonly)
      } yield (A.split(l, r), A.split(l, r) isDefinedAt (nums))
      (definedness.find(_._2).get._1(nums)
       must be_===((0 - num, 0 - num2)))
  }
}
