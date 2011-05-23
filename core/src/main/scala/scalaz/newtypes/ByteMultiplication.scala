package scalaz
package newtypes

import scalaz.{Newtype, Pack, Unpack}

sealed trait ByteMultiplication extends Pimp[Byte]

object ByteMultiplication extends ByteMultiplications

trait ByteMultiplications {
  implicit val ByteMultiplicationUnpack: Unpack[ByteMultiplication, Byte] = new Unpack[ByteMultiplication, Byte] {
    val unpack = (_: ByteMultiplication).value
  }

  implicit val ByteMultiplicationPack: Pack[ByteMultiplication, Byte] = new Pack[ByteMultiplication, Byte] {
    val pack = (b: Byte) => new ByteMultiplication {
      val value = b
    }
  }

  implicit val ByteMultiplicationNewtype: Newtype[ByteMultiplication, Byte] =
    Newtype.newtype

  implicit def ByteMultiplicationZero: Zero[ByteMultiplication] =
    Zero.zero(implicitly[Pack[ByteMultiplication, Byte]].pack(1))

  implicit def ByteMultiplicationSemigroup: Semigroup[ByteMultiplication] = new Semigroup[ByteMultiplication] {
    def append(a1: ByteMultiplication, a2: => ByteMultiplication) =
      implicitly[Pack[ByteMultiplication, Byte]].pack((a1.value.toInt * a2.value.toInt).toByte)
  }

  implicit def ByteMultiplicationMonoid: Monoid[ByteMultiplication] =
    Monoid.monoid

  implicit def ByteMultiplicationShow: Show[ByteMultiplication] =
    Show.UnpackShow[ByteMultiplication, Byte]

  implicit def ByteMultiplicationEqual: Equal[ByteMultiplication] =
    Equal.UnpackEqual[ByteMultiplication, Byte]

  implicit def ByteMultiplicationOrder: Order[ByteMultiplication] =
    Order.UnpackOrder[ByteMultiplication, Byte]

}
