package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

object ProductTest extends SpecLite {
  type OptionList[α] = (Option[α], List[α])
  type OneAndOption[α] = OneAnd[Option, α]
  type OneAndOptionPair[α] = (OneAndOption[α], OneAndOption[α])
  type EitherTuple2[a, b] = (Either[a, b], (a, b))

  implicit val optionListMonadPlus: MonadPlus[OptionList] = MonadPlus[Option].product[List]
  implicit val optionListZip: Zip[OptionList] = Zip[Option].product[List]
  implicit val oneAndOptionPairTraverse1: Traverse1[OneAndOptionPair] = Traverse1[OneAndOption].product[OneAndOption]

  {
    implicit val optionListBindRec: BindRec[OptionList] = BindRec[Option].product[List]
    checkAll(bindRec.laws[OptionList])
  }

  checkAll(monadPlus.strongLaws[OptionList])
  checkAll(zip.laws[OptionList])
  checkAll(traverse1.laws[OneAndOptionPair])

  implicit val eitherTuple2: Bitraverse[EitherTuple2] = Bitraverse[Either].product[Tuple2]
  checkAll(bitraverse.laws[EitherTuple2])
}
