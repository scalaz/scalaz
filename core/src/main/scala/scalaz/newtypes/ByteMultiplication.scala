package scalaz
package newtypes

sealed trait ByteMultiplication {
  val value: Byte
}

object ByteMultiplication extends ByteMultiplications

trait ByteMultiplications {
  implicit val ByteMultiplication_^*^ : ^*^[ByteMultiplication, Byte] =
    ^*^.^*^(_.value, b => new ByteMultiplication {
      val value = b
    })

  implicit def ByteMultiplicationZero: Zero[ByteMultiplication] =
    Zero.zero(implicitly[^*^[ByteMultiplication, Byte]].pack(1))

  implicit def ByteMultiplicationSemigroup: Semigroup[ByteMultiplication] = new Semigroup[ByteMultiplication] {
    def append(a1: ByteMultiplication, a2: => ByteMultiplication) =
      implicitly[^*^[ByteMultiplication, Byte]].pack((a1.value.toInt * a2.value.toInt).toByte)
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
