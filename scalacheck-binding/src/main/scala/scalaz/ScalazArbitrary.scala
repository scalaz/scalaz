package scalaz

import java.math.BigInteger
import org.scalacheck.{Gen, Arbitrary}

object ScalazArbitrary {
  import Scalaz._
  import Arbitrary._
  import Gen._  
  import ScalaCheckImplicits._  

  private def arb[A: Arbitrary]: Arbitrary[A] = implicitly[Arbitrary[A]]

  implicit def AlphaArbitrary: Arbitrary[Alpha] = Arbitrary(oneOf(alphas ∘ (value _): _*))

  implicit def BooleanConjunctionArbitrary: Arbitrary[BooleanConjunction] = arb[Boolean] ∘ ((_: Boolean).|∧|)

  implicit def arbBigInt: Arbitrary[BigInt] = arb[Int] <⊛> (arb[Int], (_: Int) * (_: Int))

  implicit def arbBigInteger: Arbitrary[BigInteger] = arb[BigInt] ∘ (_.bigInteger)

  implicit def BigIntegerMultiplicationArbitrary: Arbitrary[BigIntegerMultiplication] = arb[BigInteger] ∘ ((_: BigInteger).∏)

  implicit def BigIntMultiplicationArbitrary: Arbitrary[BigIntMultiplication] = arb[BigInt] ∘ ((_: BigInt).∏)

  implicit def ByteMultiplicationArbitrary: Arbitrary[ByteMultiplication] = arb[Byte] ∘ ((_: Byte).∏)

  implicit def CharMultiplicationArbitrary: Arbitrary[CharMultiplication] = arb[Char] ∘ ((_: Char).∏)

  implicit def ShortMultiplicationArbitrary: Arbitrary[ShortMultiplication] = arb[Short] ∘ ((_: Short).∏)

  implicit def LongMultiplicationArbitrary: Arbitrary[LongMultiplication] = arb[Long] ∘ ((_: Long).∏)

  implicit def DigitArbitrary: Arbitrary[Digit] = Arbitrary(oneOf(digits ∘ (value _): _*))

  implicit def DListArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[DList[A]] = arb[List[A]] ∘ (as => dlist(_ => as))

  implicit def NonEmptyListArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[NonEmptyList[A]] = arb[A] <⊛> (arb[List[A]], nel(_: A, _: List[A]))

  implicit def OrderingArbitrary: Arbitrary[Ordering] = Arbitrary(oneOf(LT, EQ, GT))
  
  implicit def TreeArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Tree[A]] = Arbitrary {
    def tree(n: Int): Gen[Tree[A]] = n match {
      case 0 => arbitrary[A] ∘ (leaf _)
      case n => {
        val nextSize = n.abs / 2
        arbitrary[A] <⊛> (resize(n, arbStream[Tree[A]](Arbitrary(tree(nextSize))).arbitrary), ((a: A, f: Stream[Tree[A]]) => node(a, f)))
      }
    }
    Gen.sized(tree _)
  }
  
  implicit def TreeLocArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[TreeLoc[A]] = arb[Tree[A]] ∘ ((t: Tree[A]) => t.loc)

  implicit def ValidationArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Validation[A, B]] = arb[Either[A, B]] ∘ (validation _)
}