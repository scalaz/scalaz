package scalaz
package std.java.math

import java.math.BigInteger

trait BigIntegerInstances {
  implicit val bigIntegerInstance: Ring[BigInteger] with Enum[BigInteger] with Show[BigInteger] = new Ring[BigInteger] with Enum[BigInteger] with Show[BigInteger] {
    override def shows(f: BigInteger) = f.toString

    def append(f1: BigInteger, f2: => BigInteger) = f1 add f2

    def multiply(f1: BigInteger, f2: ⇒ BigInteger) = f1 multiply f2

    def zero = BigInteger.ZERO

    def one = BigInteger.ONE

    def inverse(f: BigInteger) = f.negate()

    def succ(b: BigInteger) = b add BigInteger.ONE
    def pred(b: BigInteger) = b subtract BigInteger.ONE
    override def succn(a: Int, b: BigInteger) = b add BigInteger.valueOf(a)
    override def predn(a: Int, b: BigInteger) = b subtract BigInteger.valueOf(a)
    override def min = None
    override def max = None

    def order(x: BigInteger, y: BigInteger) = x.compareTo(y) match {
      case x if x < 0   => Ordering.LT
      case x if x == 0 => Ordering.EQ
      case x if x > 0   => Ordering.GT
    }

    object multiplication extends Monoid[BigInteger] {
      def append(f1: BigInteger, f2: => BigInteger) = f1 multiply f2

      def zero: BigInteger = BigInteger.ONE
    }
  }

  import Tags.Multiplication

  implicit val bigIntegerMultiplication: Monoid[BigInteger @@ Multiplication] with Order[BigInteger @@ Multiplication] with Show[BigInteger @@ Multiplication] = new Monoid[BigInteger @@ Multiplication] with Order[BigInteger @@ Multiplication] with Show[BigInteger @@ Multiplication] {
    override def shows(f: scalaz.@@[BigInteger, Multiplication]) = f.toString

    def append(f1: BigInteger @@ Multiplication, f2: => BigInteger @@ Multiplication) = Multiplication(f1 multiply f2)

    def zero: BigInteger @@ Multiplication = Multiplication(BigInteger.ONE)

    def order(x: BigInteger @@ Multiplication, y: BigInteger @@ Multiplication) = x.compareTo(y) match {
      case x if x < 0   => Ordering.LT
      case x if x == 0 => Ordering.EQ
      case x if x > 0   => Ordering.GT
    }
  }
}

object bigInteger extends BigIntegerInstances {

}
