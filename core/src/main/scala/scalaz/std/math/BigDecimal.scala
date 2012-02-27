package scalaz
package std
package math

trait BigDecimalInstances {
  implicit val bigDecimalInstance: Monoid[BigDecimal] with Enum[BigDecimal] with Show[BigDecimal] = new Monoid[BigDecimal] with Enum[BigDecimal] with Show[BigDecimal] {
    def show(f: BigDecimal) = f.toString.toList

    def append(f1: BigDecimal, f2: => BigDecimal) = f1 + f2

    def zero = 0L

    def order(x: BigDecimal, y: BigDecimal) = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    def succ(b: BigDecimal) = b + 1
    def pred(b: BigDecimal) = b - 1
    override def succn(a: Int, b: BigDecimal) = b + a
    override def predn(a: Int, b: BigDecimal) = b - a
    override def min = None
    override def max = None


    object multiplication extends Monoid[BigDecimal] {
      def append(f1: BigDecimal, f2: => BigDecimal): BigDecimal = f1 * f2

      def zero: BigDecimal = 1
    }
  }

  import Tags.Multiplication

  implicit val BigDecimalMultiplicationNewType: Monoid[BigDecimal @@ Multiplication] = new Monoid[BigDecimal @@ Multiplication] {
    def append(f1: BigDecimal @@ Multiplication, f2: => BigDecimal @@ Multiplication) = Multiplication(f1 * f2)

    def zero = Multiplication(1)
  }
}

object bigDecimal extends BigDecimalInstances