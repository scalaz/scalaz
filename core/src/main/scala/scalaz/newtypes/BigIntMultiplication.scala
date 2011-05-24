package scalaz
package newtypes

sealed trait BigIntMultiplication {
  val value: BigInt
}

object BigIntMultiplication extends BigIntMultiplications

trait BigIntMultiplications {
  implicit val BigIntMultiplicationNewtype: Newtype[BigIntMultiplication, BigInt] =
    Newtype.newtype(_.value, b => new BigIntMultiplication {
      val value = b
    })

  implicit def BigIntMultiplicationZero: Zero[BigIntMultiplication] =
    Zero.zero(implicitly[Newtype[BigIntMultiplication, BigInt]].pack(1))

  implicit def BigIntMultiplicationSemigroup: Semigroup[BigIntMultiplication] = new Semigroup[BigIntMultiplication] {
    def append(a1: BigIntMultiplication, a2: => BigIntMultiplication) =
      implicitly[Newtype[BigIntMultiplication, BigInt]].pack(a1.value * a2.value)
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
