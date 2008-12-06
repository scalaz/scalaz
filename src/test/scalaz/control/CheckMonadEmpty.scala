package scalaz.control

import fjs.test.Property._
import fj.test.CheckResult.summaryEx
import fjs.test.Arbitrary.SFInvariant._
import fjs.test.Arbitrary.arbUSASCIIString
import MonadEmptyLaws.{leftEmptyIdentity, rightEmptyIdentity}

object CheckMonadEmpty {
  val props = List(leftEmptyIdentity[Option, Int, String],
                   rightEmptyIdentity[Option, Int],
                   leftEmptyIdentity[List, Int, String],
                   rightEmptyIdentity[List, Int],
                   leftEmptyIdentity[Stream, Int, String],
                   rightEmptyIdentity[Stream, Int],
                   leftEmptyIdentity[Array, Int, String],
                   rightEmptyIdentity[Array, Int])
  
  def run = props foreach (p => summaryEx println +p)
  def main(args: Array[String]) = run
}
