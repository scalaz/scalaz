package scalaz
package newtypes

import scalaz.{Newtype, Pack, Unpack}

sealed trait IntMultiplication extends Pimp[Int]

object IntMultiplication extends IntMultiplications

trait IntMultiplications {
  implicit val IntMultiplicationUnpack: Unpack[IntMultiplication, Int] = new Unpack[IntMultiplication, Int] {
    val unpack = (_: IntMultiplication).value
  }

  implicit val IntMultiplicationPack: Pack[IntMultiplication, Int] = new Pack[IntMultiplication, Int] {
    val pack = (b: Int) => new IntMultiplication {
      val value = b
    }
  }

  implicit val IntMultiplicationNewtype: Newtype[IntMultiplication, Int] =
    Newtype.newtype

  implicit def IntMultiplicationZero: Zero[IntMultiplication] =
    Zero.zero(implicitly[Pack[IntMultiplication, Int]].pack(1))

  implicit def IntMultiplicationSemigroup: Semigroup[IntMultiplication] = new Semigroup[IntMultiplication] {
    def append(a1: IntMultiplication, a2: => IntMultiplication) =
      implicitly[Pack[IntMultiplication, Int]].pack(a1.value * a2.value)
  }

  implicit def IntMultiplicationMonoid: Monoid[IntMultiplication] =
    Monoid.monoid

  implicit def IntMultiplicationShow: Show[IntMultiplication] =
    Show.UnpackShow[IntMultiplication, Int]

  implicit def IntMultiplicationEqual: Equal[IntMultiplication] =
    Equal.UnpackEqual[IntMultiplication, Int]

  implicit def IntMultiplicationOrder: Order[IntMultiplication] =
    Order.UnpackOrder[IntMultiplication, Int]

}
