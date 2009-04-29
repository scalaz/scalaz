package scalaz

sealed trait IntW {
  val value: Int

  import S._
  
  def |*| = IntMultiplication.multiplication(value)

  def ordering = if(value < 0) LT else if(value > 0) GT else EQ

  import test._

  def >-->(high: Int): Gen[Int] = if(value < high) Gen.fail else Gen.randomised(_.chooseInt(value, high).gen)
}

object IntW {
  implicit def IntTo(n: Int): IntW = new IntW {
    val value = n
  }

  implicit def IntFrom(n: IntW) = n.value
}
