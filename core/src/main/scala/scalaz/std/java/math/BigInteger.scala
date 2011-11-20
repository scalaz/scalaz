package scalaz
package std.java.math

import java.math.BigInteger

trait BigIntegers {
  implicit object bigIntegerInstance extends Monoid[BigInteger] with Order[BigInteger] with Show[BigInteger] {
    def show(f: BigInteger): List[Char] = f.toString.toList

    def append(f1: BigInteger, f2: => BigInteger): BigInteger = f1 add f2

    def zero: BigInteger = BigInteger.ZERO

    def order(x: BigInteger, y: BigInteger): Ordering = x.compareTo(y) match {
      case x if x < 0   => Ordering.LT
      case x if x == 0 => Ordering.EQ
      case x if x > 0   => Ordering.GT
    }

    object multiplication extends Monoid[BigInteger] {
      def append(f1: BigInteger, f2: => BigInteger): BigInteger = f1 multiply f2

      def zero: BigInteger = BigInteger.ONE
    }
  }

  import Tags.Multiplication

  implicit object bigIntegerMultiplication extends Monoid[BigInteger @@ Multiplication] with Order[BigInteger @@ Multiplication] with Show[BigInteger @@ Multiplication] {
    def show(f: scalaz.@@[BigInteger, Multiplication]) = f.toString.toList

    def append(f1: BigInteger @@ Multiplication, f2: => BigInteger @@ Multiplication): BigInteger @@ Multiplication = Tag(f1 multiply f2)

    def zero: BigInteger @@ Multiplication = Tag(BigInteger.ONE)

    def order(x: BigInteger @@ Multiplication, y: BigInteger @@ Multiplication): Ordering = x.compareTo(y) match {
      case x if x < 0   => Ordering.LT
      case x if x == 0 => Ordering.EQ
      case x if x > 0   => Ordering.GT
    }
  }
}

object bigInteger extends BigIntegers {

}
