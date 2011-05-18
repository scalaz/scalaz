package scalaz
package newtypes

import scalaz.{Newtype, Pack, Unpack}

sealed trait LongMultiplication {
  val value: Long
}

object LongMultiplication extends LongMultiplications

trait LongMultiplications {
  implicit val LongMultiplicationUnpack: Unpack[LongMultiplication, Long] = new Unpack[LongMultiplication, Long] {
    val unpack = (_: LongMultiplication).value
  }

  implicit val LongMultiplicationPack: Pack[LongMultiplication, Long] = new Pack[LongMultiplication, Long] {
    val pack = (b: Long) => new LongMultiplication {
      val value = b
    }
  }

  implicit val LongMultiplicationNewtype: Newtype[LongMultiplication, Long] =
    Newtype.newtype

  implicit def LongMultiplicationZero: Zero[LongMultiplication] =
    Zero.zero(implicitly[Pack[LongMultiplication, Long]].pack(1))

  implicit def LongMultiplicationSemigroup: Semigroup[LongMultiplication] = new Semigroup[LongMultiplication] {
    def append(a1: LongMultiplication, a2: => LongMultiplication) =
      implicitly[Pack[LongMultiplication, Long]].pack(a1.value * a2.value)
  }

  implicit def LongMultiplicationMonoid: Monoid[LongMultiplication] =
    Monoid.monoid

  implicit def LongMultiplicationShow: Show[LongMultiplication] =
    Show.UnpackShow[LongMultiplication, Long]

  implicit def LongMultiplicationEqual: Equal[LongMultiplication] =
    Equal.UnpackEqual[LongMultiplication, Long]

  implicit def LongMultiplicationOrder: Order[LongMultiplication] =
    Order.UnpackOrder[LongMultiplication, Long]

}
