package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object ProductTest extends SpecLite {
  type OptionList[α] = (Option[α], List[α])
  type OneAndOption[α] = OneAnd[Option, α]
  type OneAndOptionPair[α] = (OneAndOption[α], OneAndOption[α])

  implicit val optionListApplicativePlus = ApplicativePlus[Option].product[List]
  implicit val optionListZip = Zip[Option].product[List]
  implicit val oneAndOptionPairTraverse1 = Traverse1[OneAndOption].product[OneAndOption]

  checkAll(applicative.laws[OptionList])
  checkAll(plusEmpty.laws[OptionList])
  checkAll(zip.laws[OptionList])
  checkAll(traverse1.laws[OneAndOptionPair])

  implicit val eitherTuple2 = Bitraverse[Either].product[Tuple2]
  checkAll(bitraverse.laws[λ[(α, β) => (Either[α, β], (α, β))]])
}
