package scalaz
package newtypes

import scalaz.{Newtype, Pack, Unpack}
import java.math.BigInteger

sealed trait BigIntegerMultiplication {
  val value: BigInteger
}

object BigIntegerMultiplication extends BigIntegerMultiplications

trait BigIntegerMultiplications {
  implicit val BigIntegerMultiplicationUnpack: Unpack[BigIntegerMultiplication, BigInteger] = new Unpack[BigIntegerMultiplication, BigInteger] {
    val unpack = (_: BigIntegerMultiplication).value
  }

  implicit val BigIntegerMultiplicationPack: Pack[BigIntegerMultiplication, BigInteger] = new Pack[BigIntegerMultiplication, BigInteger] {
    val pack = (b: BigInteger) => new BigIntegerMultiplication {
      val value = b
    }
  }

  implicit val BigIntegerMultiplicationNewtype: Newtype[BigIntegerMultiplication, BigInteger] =
    Newtype.newtype

  implicit def BigIntegerMultiplicationZero: Zero[BigIntegerMultiplication] =
    Zero.zero(implicitly[Pack[BigIntegerMultiplication, BigInteger]].pack(java.math.BigInteger.valueOf(0)))

  implicit def BigIntegerMultiplicationSemigroup: Semigroup[BigIntegerMultiplication] = new Semigroup[BigIntegerMultiplication] {
    def append(a1: BigIntegerMultiplication, a2: => BigIntegerMultiplication) =
      implicitly[Pack[BigIntegerMultiplication, BigInteger]].pack(a1.value multiply a2.value)
  }

  implicit def BigIntegerMultiplicationMonoid: Monoid[BigIntegerMultiplication] =
    Monoid.monoid

  implicit def BigIntegerMultiplicationShow: Show[BigIntegerMultiplication] =
    Show.UnpackShow[BigIntegerMultiplication, java.math.BigInteger]

  implicit def BigIntegerMultiplicationEqual: Equal[BigIntegerMultiplication] =
    Equal.UnpackEqual[BigIntegerMultiplication, java.math.BigInteger]

  implicit def BigIntegerMultiplicationOrder: Order[BigIntegerMultiplication] =
    Order.UnpackOrder[BigIntegerMultiplication, java.math.BigInteger]

}
