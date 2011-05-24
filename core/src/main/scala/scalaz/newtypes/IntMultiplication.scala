package scalaz
package newtypes

sealed trait IntMultiplication {
  val value: Int
}

object IntMultiplication extends IntMultiplications

trait IntMultiplications {
  implicit val IntMultiplication_^*^ : ^*^[IntMultiplication, Int] =
    ^*^.^*^(_.value, b => new IntMultiplication {
      val value = b
    })

  implicit def IntMultiplicationZero: Zero[IntMultiplication] =
    Zero.zero(implicitly[^*^[IntMultiplication, Int]].pack(1))

  implicit def IntMultiplicationSemigroup: Semigroup[IntMultiplication] = new Semigroup[IntMultiplication] {
    def append(a1: IntMultiplication, a2: => IntMultiplication) =
      implicitly[^*^[IntMultiplication, Int]].pack(a1.value * a2.value)
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
