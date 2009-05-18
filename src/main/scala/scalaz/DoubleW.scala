package scalaz

sealed trait DoubleW {
  val value: Double

  import S._

  def ordering = if(value < 0D) LT else if(value > 0D) GT else EQ

  import test._

  def >-->(high: Double): Gen[Double] = if(value > high) Gen.fail else Gen.randomised(_.chooseDouble(value, high).gen)
}

object DoubleW {
  implicit def DoubleTo(n: Double): DoubleW = new DoubleW {
    val value = n
  }

  implicit def DoubleFrom(n: DoubleW) = n.value
}
