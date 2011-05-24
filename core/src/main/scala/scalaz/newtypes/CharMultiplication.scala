package scalaz
package newtypes

sealed trait CharMultiplication {
  val value: Char
}

object CharMultiplication extends CharMultiplications

trait CharMultiplications {
  implicit val CharMultiplicationNewtype: Newtype[CharMultiplication, Char] =
    Newtype.newtype(_.value, b => new CharMultiplication {
      val value = b
    })

  implicit def CharMultiplicationZero: Zero[CharMultiplication] =
    Zero.zero(implicitly[Newtype[CharMultiplication, Char]].pack(1))

  implicit def CharMultiplicationSemigroup: Semigroup[CharMultiplication] = new Semigroup[CharMultiplication] {
    def append(a1: CharMultiplication, a2: => CharMultiplication) =
      implicitly[Newtype[CharMultiplication, Char]].pack((a1.value.toInt * a2.value.toInt).toChar)
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
