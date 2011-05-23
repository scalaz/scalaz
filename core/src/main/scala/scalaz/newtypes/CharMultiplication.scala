package scalaz
package newtypes

import scalaz.{Newtype, Pack, Unpack}

sealed trait CharMultiplication extends Pimp[Char]

object CharMultiplication extends CharMultiplications

trait CharMultiplications {
  implicit val CharMultiplicationUnpack: Unpack[CharMultiplication, Char] = new Unpack[CharMultiplication, Char] {
    val unpack = (_: CharMultiplication).value
  }

  implicit val CharMultiplicationPack: Pack[CharMultiplication, Char] = new Pack[CharMultiplication, Char] {
    val pack = (b: Char) => new CharMultiplication {
      val value = b
    }
  }

  implicit val CharMultiplicationNewtype: Newtype[CharMultiplication, Char] =
    Newtype.newtype

  implicit def CharMultiplicationZero: Zero[CharMultiplication] =
    Zero.zero(implicitly[Pack[CharMultiplication, Char]].pack(1))

  implicit def CharMultiplicationSemigroup: Semigroup[CharMultiplication] = new Semigroup[CharMultiplication] {
    def append(a1: CharMultiplication, a2: => CharMultiplication) =
      implicitly[Pack[CharMultiplication, Char]].pack((a1.value.toInt * a2.value.toInt).toChar)
  }

  implicit def CharMultiplicationMonoid: Monoid[CharMultiplication] =
    Monoid.monoid

  implicit def CharMultiplicationShow: Show[CharMultiplication] =
    Show.UnpackShow[CharMultiplication, Char]

  implicit def CharMultiplicationEqual: Equal[CharMultiplication] =
    Equal.UnpackEqual[CharMultiplication, Char]

  implicit def CharMultiplicationOrder: Order[CharMultiplication] =
    Order.UnpackOrder[CharMultiplication, Char]

}
