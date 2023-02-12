package scalaz
package std.java.math

import java.math.BigDecimal

trait BigDecimalInstances {
  implicit val javaBigDecimalInstance: Show[BigDecimal] with Equal[BigDecimal] = new Show[BigDecimal] with Equal[BigDecimal] {
    override def show(f: BigDecimal): Cord = Cord(shows(f))
    override def shows(f: BigDecimal): String = f.toString
    override def equal(x: BigDecimal, y: BigDecimal): Boolean = x.equals(y)
  }
}

object bigDecimal extends BigDecimalInstances
