package scalaz.control

import fjs.test.Property._
import fj.test.CheckResult.summaryEx
import fjs.test.Arbitrary.SFInvariant._
import fjs.test.Arbitrary.arbUSASCIIString
import ApplicativeLaws.{identity, composition, homomorphism, interchange}
import list.NonEmptyList
import list.ArbitraryNonEmptyList._
import validation.Validation
import validation.ArbitraryValidation._

object CheckApplicative {
  val props = List(identity[Option, Int],
                   composition[Option, Int, Long, String],
                   homomorphism[Option, Int, String],
                   interchange[Option, Int, String],
                   identity[List, Int],
                   composition[List, Int, Long, String],
                   homomorphism[List, Int, String],
                   interchange[List, Int, String],
                   identity[Stream, Int],
                   composition[Stream, Int, Long, String],
                   homomorphism[Stream, Int, String],
                   interchange[Stream, Int, String],
                   // ClassCastException when testing Array, why?
                   // identity[Array, Int],
                   // composition[Array, Int, Long, String],
                   // homomorphism[Array, Int, String],
                   // interchange[Array, Int, String],
                   identity[NonEmptyList, Int],
                   composition[NonEmptyList, Int, Long, String],
                   homomorphism[NonEmptyList, Int, String],
                   interchange[NonEmptyList, Int, String],
                   identity[PartialType[Either, String]#Apply, Int],
                   composition[PartialType[Either, String]#Apply, Int, Long, String],
                   homomorphism[PartialType[Either, String]#Apply, Int, String],
                   interchange[PartialType[Either, String]#Apply, Int, String],
                   identity[PartialType[Validation, String]#Apply, Int],
                   composition[PartialType[Validation, String]#Apply, Int, Long, String],
                   homomorphism[PartialType[Validation, String]#Apply, Int, String],
                   interchange[PartialType[Validation, String]#Apply, Int, String])

  def run = props foreach (p => summaryEx println +p)
  def main(args: Array[String]) = run  
}
