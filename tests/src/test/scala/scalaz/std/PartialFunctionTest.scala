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
}
