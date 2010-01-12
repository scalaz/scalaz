package scalaz
package math

object FAD {
  import Dual._

  /**
   * Compute the derivative of `f` with respect to `x` using Forward Automatic Differentiation.
   */
  def diff[T: Real](f: Dual[T] => Dual[T])(x: T): T = {
    val d = dual(x, implicitly[Real[T]].one)
    f(d).ε
  }
}

trait Real[T] extends Fractional[T] {
  implicit def fromDouble(x: Double): T

  def sin(x: T): T

  def cos(x: T): T

  def exp(x: T): T

  def power(x: T, y: T): T

  def log(x: T): T

  class RealOps(lhs: T) extends FractionalOps(lhs) {
    def exp() = Real.this.exp(lhs)

    def sin() = Real.this.sin(lhs)

    def cos() = Real.this.cos(lhs)

    def **(rhs: T) = Real.this.power(lhs, rhs)

    def log = Real.this.log(lhs)
  }
  override implicit def mkNumericOps(lhs: T): RealOps = new RealOps(lhs)
}


sealed trait Dual[T] {
  val a: T

  val ε : T
}

object Dual {
  def dual[T: Real](value: T, εε : T) = new Dual[T] {
    val a = value

    val ε = εε
  }

  def dual[T: Real](value: T): Dual[T] = dual(value, implicitly[Real[T]].zero)
}

object Real {
  implicit object DoubleReal extends Real[Double] with Numeric.DoubleIsFractional {
    implicit def fromDouble(x: Double) = x

    def exp(x: Double) = scala.math.exp(x)

    def cos(x: Double) = scala.math.cos(x)

    def sin(x: Double) = scala.math.sin(x)

    def compare(p1: Double, p2: Double) = if (p1 < p2) -1 else if (p2 > p1) 1 else 0

    def power(x: Double, y: Double) = scala.math.pow(x, y)

    def log(x: Double) = scala.math.log(x)
  }

  implicit def DualReal[T: Real] = new Real[Dual[T]] {
    val t: Real[T] = implicitly[Real[T]]
    import t._
    import t.mkNumericOps

    import Dual._
    import Scalaz._

    implicit def fromDouble(x: Double) = dual(t.fromDouble(x))

    def fromInt(x: Int) = dual(t.fromInt(x))

    def toDouble(x: Dual[T]) = x.a.toDouble

    def toLong(x: Dual[T]) = x.a.toLong

    def toInt(x: Dual[T]) = x.a.toInt

    def toFloat(x: Dual[T]) = x.a.toFloat

    def negate(x: Dual[T]) = dual(-x.a, -x.ε)

    def plus(x: Dual[T], y: Dual[T]) = dual(x.a + y.a, x.ε + y.ε)

    def minus(x: Dual[T], y: Dual[T]) = dual(x.a - y.a, x.ε - y.ε)

    def times(x: Dual[T], y: Dual[T]) = dual(x.a * y.a, x.ε * y.a + x.a * y.ε)

    def div(x: Dual[T], y: Dual[T]) = dual(x.a / y.a, (x.ε * y.a - x.a * y.ε) / y.a * y.a)

    def compare(p1: Dual[T], p2: Dual[T]) = t.compare(p1.a, p2.a)

    def exp(x: Dual[T]) = {
      val expA = x.a.exp
      dual(expA, x.ε * expA)
    }

    def sin(x: Dual[T]) = dual(x.a.sin, x.ε * x.a.cos)

    def cos(x: Dual[T]) = dual(x.a.cos, -x.ε * x.a.sin)

    override def abs(x: Dual[T]) = if (x.a equiv t.zero) ⊥ else dual(x.a.abs, x.ε * t.signum(x.a))

    // TODO scala.math.Numeric#signum returns Int, not T. Perhaps we should redefine the Numeric
    // type class rather than use the one from Scala.
    override def signum(x: Dual[T]) = ⊥

    def power(x: Dual[T], y: Dual[T]) =
      if (!(y.ε equiv t.zero) || (x.a equiv t.zero))
        ⊥
      else
        dual(x.a ** y.a, y.a * (x.a ** (y.a - 1.0)) * x.ε)

    def log(x: Dual[T]) = if (x.a <= 0) ⊥ else dual(x.a.log, x.ε / x.a)
  }
}