package scalaz
package newtypes

import scalaz.{Newtype, Pack, Unpack}

sealed trait ShortMultiplication extends Pimp[Short]

object ShortMultiplication extends ShortMultiplications

trait ShortMultiplications {
  implicit val ShortMultiplicationUnpack: Unpack[ShortMultiplication, Short] = new Unpack[ShortMultiplication, Short] {
    val unpack = (_: ShortMultiplication).value
  }

  implicit val ShortMultiplicationPack: Pack[ShortMultiplication, Short] = new Pack[ShortMultiplication, Short] {
    val pack = (b: Short) => new ShortMultiplication {
      val value = b
    }
  }

  implicit val ShortMultiplicationNewtype: Newtype[ShortMultiplication, Short] =
    Newtype.newtype

  implicit def ShortMultiplicationZero: Zero[ShortMultiplication] =
    Zero.zero(implicitly[Pack[ShortMultiplication, Short]].pack(1))

  implicit def ShortMultiplicationSemigroup: Semigroup[ShortMultiplication] = new Semigroup[ShortMultiplication] {
    def append(a1: ShortMultiplication, a2: => ShortMultiplication) =
      implicitly[Pack[ShortMultiplication, Short]].pack((a1.value.toInt * a2.value.toInt).toShort)
  }

  implicit def ShortMultiplicationMonoid: Monoid[ShortMultiplication] =
    Monoid.monoid

  implicit def ShortMultiplicationShow: Show[ShortMultiplication] =
    Show.UnpackShow[ShortMultiplication, Short]

  implicit def ShortMultiplicationEqual: Equal[ShortMultiplication] =
    Equal.UnpackEqual[ShortMultiplication, Short]

  implicit def ShortMultiplicationOrder: Order[ShortMultiplication] =
    Order.UnpackOrder[ShortMultiplication, Short]

}
