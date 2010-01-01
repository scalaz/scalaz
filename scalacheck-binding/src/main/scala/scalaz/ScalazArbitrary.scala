package scalaz

import org.scalacheck.Arbitrary
import java.math.BigInteger
import ScalaCheckImplicits._

object ScalazArbitrary {
  import Scalaz._

  private def arb[A: Arbitrary]: Arbitrary[A] = implicitly[Arbitrary[A]]

  implicit def AlphaArbitrary: Arbitrary[Alpha] = arb[Int] ∘ ((i: Int) => alphas(i % alphas.size))

  implicit def BooleanConjunctionArbitrary: Arbitrary[BooleanConjunction] = arb[Boolean] ∘ ((_: Boolean).|∧|)

  implicit def arbBigInt: Arbitrary[BigInt] = arb[Int] <⊛> (arb[Int], (_: Int) * (_: Int))

  implicit def arbBigInteger: Arbitrary[BigInteger] = arb[BigInt] ∘ (_.bigInteger)

  implicit def BigIntegerMultiplicationArbitrary: Arbitrary[BigIntegerMultiplication] = arb[BigInteger] ∘ ((_: BigInteger).∏)

  implicit def BigIntMultiplicationArbitrary: Arbitrary[BigIntMultiplication] = arb[BigInt] ∘ ((_: BigInt).∏)

  implicit def ByteMultiplicationArbitrary: Arbitrary[ByteMultiplication] = arb[Byte] ∘ ((_: Byte).∏)

  implicit def CharMultiplicationArbitrary: Arbitrary[CharMultiplication] = arb[Char] ∘ ((_: Char).∏)

  implicit def ShortMultiplicationArbitrary: Arbitrary[ShortMultiplication] = arb[Short] ∘ ((_: Short).∏)

  implicit def LongMultiplicationArbitrary: Arbitrary[LongMultiplication] = arb[Long] ∘ ((_: Long).∏)

  implicit def DigitArbitrary: Arbitrary[Digit] = arb[Int] ∘ ((i: Int) => LongDigit(i))

  implicit def NonEmptyListArbitrary[A: Arbitrary]: Arbitrary[NonEmptyList[A]] = arb[A] <⊛> (arb[List[A]], nel(_: A, _: List[A]))

  implicit def DListArbitrary[A: Arbitrary]: Arbitrary[DList[A]] = arb[List[A]] ∘ (as => dlist(_ => as))

  // todo more types here
}