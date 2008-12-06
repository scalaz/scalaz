package scalaz

import fjs.test.Property._
import fjs.test.Arbitrary.{arbSInt, arbString}
import fjs.test.Coarbitrary.coarbSInt
import fj.test.CheckResult.summaryEx
import fjs.F._
import fjs.F2._
import EitherW._
import EitherW.LeftProjectionW._
import EitherW.RightProjectionW._

object CheckEitherW {
  val prop_swap = prop((e: Either[Int, Int]) => ~e == (e match {
    case Left(a) => Right(a)
    case Right(b) => Left(b)
  }))

  val prop_if = prop((e: Either[Int, Int], x: Int, y: Int) => e ? (x, y) == (if(e.isLeft) x else y))

  val prop_valueLeft = prop((e: Either[String, Int], f: Int => String) => e.left.value(f) == e.fold(x => x, f(_)))
  val prop_valueRight = prop((e: Either[Int, String], f: Int => String) => e.right.value(f) == e.fold(f(_), x => x))

  val props = List(prop_swap, prop_if, prop_valueLeft, prop_valueRight)

  def run = props foreach (p => summaryEx println +p)
  def main(args: Array[String]) = run
}
