package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

object CompositionTest extends SpecLite {
  type OptionList[α] = Option[List[α]]

  implicit val optionListApplicative = ApplicativePlus[Option].compose[List]
  implicit val optionListTraverse = Traverse[Option].compose[List]
  implicit val oneAndOptNelTraverse = Traverse1[({type λ[α] = OneAnd[Option, α]})#λ].compose[NonEmptyList]

  checkAll(applicative.laws[OptionList])
  checkAll(plusEmpty.laws[OptionList])
  checkAll(traverse.laws[OptionList])
  checkAll(traverse1.laws[({type λ[α] = OneAnd[Option, NonEmptyList[α]]})#λ])


  implicit val eitherTuple2 = Bitraverse[Either].compose[Tuple2]
  checkAll(bitraverse.laws[({type λ[α, β]=Either[(α, β), (α, β)]})#λ])

  implicit val listEitherBitraverse = Traverse[List].bicompose[Either]
  checkAll(bitraverse.laws[({type λ[α, β] = List[Either[α, β]]})#λ])
}
