package scalaz.control

import fjs.test.Property._
import fj.test.CheckResult.summaryEx
import fjs.test.Arbitrary.arbUSASCIIString
import fjs.test.Coarbitrary.{coarbSInt, coarbString}
import FunctorLaws.{identity, composition}
import list.NonEmptyList
import list.ArbitraryNonEmptyList._

object CheckFunctor {
  val props = List(identity[Option, Int],
                   composition[Option, Int, String, Long],
                   identity[List, Int],
                   composition[List, Int, String, Long],
                   identity[Stream, Int],
                   composition[Stream, Int, String, Long],
                   identity[Array, Int],
                   composition[Array, Int, String, Long],
                   identity[NonEmptyList, Int],
                   composition[NonEmptyList, Int, String, Long],
                   identity[PartialType[Either, String]#Apply, Int],
                   composition[PartialType[Either, String]#Apply, Int, String, Long])

  def run = props foreach (p => summaryEx println +p)
  def main(args: Array[String]) = run
}
