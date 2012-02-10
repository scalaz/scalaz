package scalaz
package std
package math

trait BigInts {
  implicit val bigIntInstance: Group[BigInt] with Enum[BigInt] with Show[BigInt] = new Group[BigInt] with Enum[BigInt] with Show[BigInt] {
    def show(f: BigInt): List[Char] = f.toString.toList

    def append(f1: BigInt, f2: => BigInt): BigInt = f1 + f2

    def zero: BigInt = 0L

    def inverse(f: BigInt): BigInt = -f

    def order(x: BigInt, y: BigInt): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    def succ = (b: BigInt) => b + 1
    def pred = (b: BigInt) => b - 1
    override def succn = (a: Int) => (b: BigInt) => b + a
    override def predn = (a: Int) => (b: BigInt) => b - a
    override def min = None
    override def max = None

    object multiplication extends Monoid[BigInt] {
      def append(f1: BigInt, f2: => BigInt): BigInt = f1 * f2

      def zero: BigInt = 1
    }
  }

  import Tags.Multiplication

  implicit val bigIntMultiplication: Monoid[BigInt @@ Multiplication] with Order[BigInt @@ Multiplication] with Show[BigInt @@ Multiplication] = new Monoid[BigInt @@ Multiplication] with Order[BigInt @@ Multiplication] with Show[BigInt @@ Multiplication] {
    def show(f: scalaz.@@[BigInt, Multiplication]) = f.toString.toList

    def append(f1: BigInt @@ Multiplication, f2: => BigInt @@ Multiplication): BigInt @@ Multiplication = Multiplication(f1 * f2)

    def zero: BigInt @@ Multiplication = Multiplication(1)

    def order(x: BigInt @@ Multiplication, y: BigInt @@ Multiplication): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT
  }
}

object bigInt extends BigInts {

}
