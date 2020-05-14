package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

object CompositionTest extends SpecLite {
  type OptionList[α] = Option[List[α]]

  implicit val optionListApplicative: ApplicativePlus[λ[α => Option[List[α]]]] = ApplicativePlus[Option].compose[List]
  implicit val optionListTraverse: Traverse[λ[α => Option[List[α]]]] = Traverse[Option].compose[List]
  implicit val oneAndOptNelTraverse: Traverse1[λ[α => OneAnd[Option, NonEmptyList[α]]]] = Traverse1[OneAnd[Option, *]].compose[NonEmptyList]

  checkAll(applicative.laws[OptionList])
  checkAll(plusEmpty.laws[OptionList])
  checkAll(traverse.laws[OptionList])
  checkAll(traverse1.laws[λ[α => OneAnd[Option, NonEmptyList[α]]]])

  implicit val eitherTuple2: Bitraverse[λ[(α, β) => Either[(α, β), (α, β)]]]  = Bitraverse[Either].compose[Tuple2]
  checkAll(bitraverse.laws[λ[(α, β) => Either[(α, β), (α, β)]]])

  implicit val listEitherBitraverse: Bitraverse[λ[(α, β) => List[Either[α, β]]]] = Traverse[List].bicompose[Either]
  checkAll(bitraverse.laws[λ[(α, β) => List[Either[α, β]]]])
}
