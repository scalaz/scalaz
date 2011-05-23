package scalaz
package newtypes

sealed trait BooleanConjunction extends Pimp[Boolean]

object BooleanConjunction extends BooleanConjunctions

trait BooleanConjunctions {
  implicit val BooleanConjunctionUnpack: Unpack[BooleanConjunction, Boolean] = new Unpack[BooleanConjunction, Boolean] {
    val unpack = (_: BooleanConjunction).value
  }

  implicit val BooleanConjunctionPack: Pack[BooleanConjunction, Boolean] = new Pack[BooleanConjunction, Boolean] {
    val pack = (b: Boolean) => new BooleanConjunction {
      val value = b
    }
  }

  implicit val BooleanConjunctionNewtype: Newtype[BooleanConjunction, Boolean] =
    Newtype.newtype

  implicit val BooleanConjunctionZero: Zero[BooleanConjunction] = new Zero[BooleanConjunction] {
    val zero = implicitly[Pack[BooleanConjunction, Boolean]].pack(true)
  }

  implicit val BooleanConjunctionSemigroup: Semigroup[BooleanConjunction] = new Semigroup[BooleanConjunction] {
    def append(a1: BooleanConjunction, a2: => BooleanConjunction) =
      implicitly[Pack[BooleanConjunction, Boolean]].pack(a1.value && a2.value)
  }

  implicit val BooleanConjunctionMonoid: Monoid[BooleanConjunction] =
    Monoid.monoid

  implicit def BooleanConjunctionShow: Show[BooleanConjunction] =
    Show.UnpackShow[BooleanConjunction, Boolean]

  implicit def BooleanConjunctionEqual: Equal[BooleanConjunction] =
    Equal.UnpackEqual[BooleanConjunction, Boolean]

  implicit def BooleanConjunctionOrder: Order[BooleanConjunction] =
    Order.UnpackOrder[BooleanConjunction, Boolean]
}
