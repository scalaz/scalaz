package scalaz

sealed trait IntW extends PimpedType[Int] {
  import Scalaz._

  def ∏ : IntMultiplication = multiplication(value)

  def ordering: Ordering = if (value < 0) LT else if (value > 0) GT else EQ

  def times[M:Monoid](m: M): M = {
    def timesPrime(a: M, n: Int): M = if (n > 0) timesPrime(m |+| a, n - 1) else a
    timesPrime(mzero, value)
  }
}

trait Ints {
  implicit def IntTo(n: Int): IntW = new IntW {
    val value = n
  }
}
