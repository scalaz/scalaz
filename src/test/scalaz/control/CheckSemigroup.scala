package scalaz.control

import fjs.test.Property._
import fj.test.CheckResult.summaryEx
import SemigroupLaws.{associative}
import list.NonEmptyList
import list.ArbitraryNonEmptyList._
 
object CheckSemigroup {
  val props = List(associative[Option[Int]],
                   associative[List[Int]],
                   associative[Stream[Int]],
                   associative[Array[Int]],
                   associative[NonEmptyList[Int]])
  
  def run = props foreach (p => summaryEx println +p)
  def main(args: Array[String]) = run
}
