package scalaz
package std
package math

trait BigInts {
  implicit object bigIntInstance extends Monoid[BigInt] with Order[BigInt] with Show[BigInt] {
    def show(f: BigInt): List[Char] = f.toString.toList

    def append(f1: BigInt, f2: => BigInt): BigInt = f1 + f2

    def zero: BigInt = 0L

    def order(x: BigInt, y: BigInt): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    object multiplication extends Monoid[BigInt] {
      def append(f1: BigInt, f2: => BigInt): BigInt = f1 * f2

      def zero: BigInt = 1
    }
  }

  import Tags.Multiplication

  implicit object bigIntMultiplication extends Monoid[BigInt @@ Multiplication] with Order[BigInt @@ Multiplication] with Show[BigInt @@ Multiplication] {
    def show(f: scalaz.@@[BigInt, Multiplication]) = f.toString.toList

    def append(f1: BigInt @@ Multiplication, f2: => BigInt @@ Multiplication): BigInt @@ Multiplication = Tag(f1 * f2)

    def zero: BigInt @@ Multiplication = Tag(1)

    def order(x: BigInt @@ Multiplication, y: BigInt @@ Multiplication): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT
  }
}

object bigInt extends BigInts {

}
