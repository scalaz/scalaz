package scalaz

sealed trait DoubleW {
  val value: Double

  import S._

  def ordering = if(value < 0D) LT else if(value > 0D) GT else EQ
}

object DoubleW {
  implicit def DoubleTo(n: Double): DoubleW = new DoubleW {
    val value = n
  }

  implicit def DoubleFrom(n: DoubleW) = n.value
}
