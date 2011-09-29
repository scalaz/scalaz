package scalaz
package std
package math

trait BigInts {
  implicit object bigInt extends Monoid[BigInt] with Order[BigInt] with Show[BigInt] {
    def show(f: BigInt): List[Char] = f.toString.toList

    def append(f1: BigInt, f2: => BigInt): BigInt = f1 + f2

    def zero: BigInt = 0L

    def order(x: BigInt, y: BigInt): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    object multiplication extends Monoid[BigInt] {
      def append(f1: BigInt, f2: => BigInt): BigInt = f1 * f2

      def zero: BigInt = 1
    }
  }

  sealed trait Multiplication

  implicit object BigIntMultiplicationNewType extends Monoid[BigInt @@ Multiplication] {
    def append(f1: BigInt @@ Multiplication, f2: => BigInt @@ Multiplication): BigInt @@ Multiplication = Tag(f1 * f2)

    def zero: BigInt @@ Multiplication = Tag(1)
  }
}

object BigInt extends BigInts {

}