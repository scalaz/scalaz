package scalaz
package std.java.math

import java.math.BigInteger

trait BigIntegerInstances {
  implicit val bigIntegerInstance: Monoid[BigInteger] & Enum[BigInteger] & Show[BigInteger] = new Monoid[BigInteger] with Enum[BigInteger] with Show[BigInteger] {
    override def show(f: BigInteger): Cord = Cord(shows(f))
    override def shows(f: BigInteger): String = f.toString

    def append(f1: BigInteger, f2: => BigInteger): BigInteger = f1 add f2

    def zero: BigInteger = BigInteger.ZERO

    def succ(b: BigInteger): BigInteger = b add BigInteger.ONE
    def pred(b: BigInteger): BigInteger = b subtract BigInteger.ONE
    override def succn(a: Int, b: BigInteger): BigInteger = b add BigInteger.valueOf(a.toLong)
    override def predn(a: Int, b: BigInteger): BigInteger = b subtract BigInteger.valueOf(a.toLong)
    override def min: Option[BigInteger] = None
    override def max: Option[BigInteger] = None

    def order(x: BigInteger, y: BigInteger): Ordering = x.compareTo(y) match {
      case x if x < 0   => Ordering.LT
      case x if x == 0 => Ordering.EQ
      case x if x > 0   => Ordering.GT
    }
  }

  import Tags.Multiplication

  implicit val bigIntegerMultiplication: Monoid[BigInteger @@ Multiplication] & Order[BigInteger @@ Multiplication] & Show[BigInteger @@ Multiplication] = new Monoid[BigInteger @@ Multiplication] with Order[BigInteger @@ Multiplication] with Show[BigInteger @@ Multiplication] {
    override def show(f: BigInteger @@ Multiplication): Cord = Cord(shows(f))
    override def shows(f: BigInteger @@ Multiplication): String = f.toString

    def append(f1: BigInteger @@ Multiplication, f2: => BigInteger @@ Multiplication): BigInteger @@ Multiplication = Multiplication(Tag.unwrap(f1) multiply Tag.unwrap(f2))

    def zero: BigInteger @@ Multiplication = Multiplication(BigInteger.ONE)

    def order(x: BigInteger @@ Multiplication, y: BigInteger @@ Multiplication): Ordering = Tag.unwrap(x).compareTo(Tag.unwrap(y)) match {
      case x if x < 0   => Ordering.LT
      case x if x == 0 => Ordering.EQ
      case x if x > 0   => Ordering.GT
    }
  }
}

object bigInteger extends BigIntegerInstances {

}
