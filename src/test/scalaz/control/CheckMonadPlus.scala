package scalaz.control

import fjs.test.Property._
import fj.test.CheckResult.summaryEx
import fjs.test.Arbitrary.SFInvariant._
import fjs.test.Arbitrary.arbUSASCIIString
import MonadPlusLaws.associative
import list.NonEmptyList
import list.ArbitraryNonEmptyList._
        
object CheckMonadPlus {
  val props = List(associative[Option, Int],
                   associative[List, Int],
                   associative[Stream, Int],
                   associative[Array, Int],
                   associative[NonEmptyList, Int],
                   associative[PartialType[Either, String]#Apply, Int])

  def run = props foreach (p => summaryEx println +p)
  def main(args: Array[String]) = run
}
