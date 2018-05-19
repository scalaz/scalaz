package scalaz
package std.java.math

import java.math.BigInteger

trait BigIntegerInstances {
  implicit val bigIntegerInstance: Monoid[BigInteger] with Enum[BigInteger] with Show[BigInteger] = new Monoid[BigInteger] with Enum[BigInteger] with Show[BigInteger] {
    override def show(f: BigInteger): Cord = Cord(shows(f))
    override def shows(f: BigInteger) = f.toString

    def append(f1: BigInteger, f2: => BigInteger) = f1 add f2

    def zero = BigInteger.ZERO

    def succ(b: BigInteger) = b add BigInteger.ONE
    def pred(b: BigInteger) = b subtract BigInteger.ONE
    override def succn(a: Int, b: BigInteger) = b add BigInteger.valueOf(a.toLong)
    override def predn(a: Int, b: BigInteger) = b subtract BigInteger.valueOf(a.toLong)
    override def min = None
    override def max = None

    def order(x: BigInteger, y: BigInteger) = x.compareTo(y) match {
      case x if x < 0   => Ordering.LT
      case x if x == 0 => Ordering.EQ
      case x if x > 0   => Ordering.GT
    }
  }

  import Tags.Multiplication

  implicit val bigIntegerMultiplication: Monoid[BigInteger @@ Multiplication] with Order[BigInteger @@ Multiplication] with Show[BigInteger @@ Multiplication] = new Monoid[BigInteger @@ Multiplication] with Order[BigInteger @@ Multiplication] with Show[BigInteger @@ Multiplication] {
    override def show(f: BigInteger @@ Multiplication): Cord = Cord(shows(f))
    override def shows(f: BigInteger @@ Multiplication) = f.toString

    def append(f1: BigInteger @@ Multiplication, f2: => BigInteger @@ Multiplication) = Multiplication(Tag.unwrap(f1) multiply Tag.unwrap(f2))

    def zero: BigInteger @@ Multiplication = Multiplication(BigInteger.ONE)

    def order(x: BigInteger @@ Multiplication, y: BigInteger @@ Multiplication) = Tag.unwrap(x).compareTo(Tag.unwrap(y)) match {
      case x if x < 0   => Ordering.LT
      case x if x == 0 => Ordering.EQ
      case x if x > 0   => Ordering.GT
    }
  }
}

object bigInteger extends BigIntegerInstances {

}
