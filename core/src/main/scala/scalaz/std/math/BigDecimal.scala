package scalaz
package std
package math

trait BigDecimals {
  implicit object bigDecimal extends Monoid[BigDecimal] with Order[BigDecimal] with Show[BigDecimal] {
    def show(f: BigDecimal): List[Char] = f.toString.toList

    def append(f1: BigDecimal, f2: => BigDecimal): BigDecimal = f1 + f2

    def zero: BigDecimal = 0L

    def order(x: BigDecimal, y: BigDecimal): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    object multiplication extends Monoid[BigDecimal] {
      def append(f1: BigDecimal, f2: => BigDecimal): BigDecimal = f1 * f2

      def zero: BigDecimal = 1
    }
  }

  sealed trait Multiplication

  implicit object BigDecimalMultiplicationNewType extends Monoid[BigDecimal @@ Multiplication] {
    def append(f1: BigDecimal @@ Multiplication, f2: => BigDecimal @@ Multiplication): BigDecimal @@ Multiplication = Tag(f1 * f2)

    def zero: BigDecimal @@ Multiplication = Tag(1)
  }
}

object BigDecimal extends BigDecimals {

}