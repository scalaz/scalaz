package scalaz
package std
package math

trait BigInts {
  implicit val bigIntInstance: Monoid[BigInt] with Enum[BigInt] with Show[BigInt] = new Monoid[BigInt] with Enum[BigInt] with Show[BigInt] {
    override def show(f: BigInt): Cord = Cord(shows(f))
    override def shows(f: BigInt) = f.toString

    def append(f1: BigInt, f2: => BigInt): BigInt = f1 + f2

    def zero: BigInt = 0L

    def order(x: BigInt, y: BigInt): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    def succ(b: BigInt) = b + 1
    def pred(b: BigInt) = b - 1
    override def succn(a: Int, b: BigInt) = b + a
    override def predn(a: Int, b: BigInt) = b - a
    override def min = None
    override def max = None
  }

  import Tags.Multiplication

  implicit val bigIntMultiplication: Monoid[BigInt @@ Multiplication] with Order[BigInt @@ Multiplication] with Show[BigInt @@ Multiplication] = new Monoid[BigInt @@ Multiplication] with Order[BigInt @@ Multiplication] with Show[BigInt @@ Multiplication] {
    override def show(f: BigInt @@ Multiplication): Cord = Cord(shows(f))
    override def shows(f: BigInt @@ Multiplication) = f.toString

    def append(f1: BigInt @@ Multiplication, f2: => BigInt @@ Multiplication): BigInt @@ Multiplication = Multiplication(Tag.unwrap(f1) * Tag.unwrap(f2))

    def zero: BigInt @@ Multiplication = Multiplication(1)

    def order(x: BigInt @@ Multiplication, y: BigInt @@ Multiplication): Ordering = if (Tag.unwrap(x) < Tag.unwrap(y)) Ordering.LT else if (Tag.unwrap(x) == Tag.unwrap(y)) Ordering.EQ else Ordering.GT
  }
}

object bigInt extends BigInts {

}
