package scalaz
package newtypes

import scalaz.{Newtype, Pack, Unpack}

sealed trait BigIntMultiplication extends Pimp[BigInt]

object BigIntMultiplication extends BigIntMultiplications

trait BigIntMultiplications {
  implicit val BigIntMultiplicationUnpack: Unpack[BigIntMultiplication, BigInt] = new Unpack[BigIntMultiplication, BigInt] {
    val unpack = (_: BigIntMultiplication).value
  }

  implicit val BigIntMultiplicationPack: Pack[BigIntMultiplication, BigInt] = new Pack[BigIntMultiplication, BigInt] {
    val pack = (b: BigInt) => new BigIntMultiplication {
      val value = b
    }
  }

  implicit val BigIntMultiplicationNewtype: Newtype[BigIntMultiplication, BigInt] =
    Newtype.newtype

  implicit def BigIntMultiplicationZero: Zero[BigIntMultiplication] =
    Zero.zero(implicitly[Pack[BigIntMultiplication, BigInt]].pack(1))

  implicit def BigIntMultiplicationSemigroup: Semigroup[BigIntMultiplication] = new Semigroup[BigIntMultiplication] {
    def append(a1: BigIntMultiplication, a2: => BigIntMultiplication) =
      implicitly[Pack[BigIntMultiplication, BigInt]].pack(a1.value * a2.value)
  }

  implicit def BigIntMultiplicationMonoid: Monoid[BigIntMultiplication] =
    Monoid.monoid

  implicit def BigIntMultiplicationShow: Show[BigIntMultiplication] =
    Show.UnpackShow[BigIntMultiplication, BigInt]

  implicit def BigIntMultiplicationEqual: Equal[BigIntMultiplication] =
    Equal.UnpackEqual[BigIntMultiplication, BigInt]

  implicit def BigIntMultiplicationOrder: Order[BigIntMultiplication] =
    Order.UnpackOrder[BigIntMultiplication, BigInt]

}
