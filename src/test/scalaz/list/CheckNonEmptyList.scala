package scalaz.list

import fjs.test.Arbitrary.SFInvariant._
import fjs.test.Arbitrary.arbUSASCIIString
import fjs.test.Property._
import fj.test.CheckResult.summaryEx
import NonEmptyList._
import ArbitraryNonEmptyList._
import EqualW._

object CheckNonEmptyList {
  val prop_head = prop((n: Int) => nel(n).head == n)

  val prop_tail = prop((x: NonEmptyList[Int]) => x.drop(1) == x.tail)

  val prop_cons = prop((x: NonEmptyList[Int], i: Int) =>
      (i <:: x).toList === i :: x)

  val prop_append = prop((x: NonEmptyList[Int], y: List[Int]) => (y <::: x).toList === y ::: x.toList)
  
  val prop_map = prop((x: NonEmptyList[Int], f: Int => String) => x.map(f).toList === x.toList.map(f))

  val prop_flatMap = prop((x: NonEmptyList[Int], f: Int => NonEmptyList[String]) => x.flatMap(f).toList === x.toList.flatMap(f(_).toList))

  val prop_toList = prop((x: NonEmptyList[Int]) => x.toList === x.head :: x.tail)

  val prop_toStream = prop((x: NonEmptyList[Int]) => x.toStream sameElements x.toList.toStream)

  val props = List(prop_head, prop_tail, prop_cons, prop_append, prop_map, prop_toList, prop_toStream)

  def run = props foreach (p => summaryEx println +p)
  def main(args: Array[String]) = run
}
