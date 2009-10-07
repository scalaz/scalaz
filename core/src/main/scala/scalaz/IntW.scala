package scalaz

sealed trait IntW {
  val value: Int

  import Scalaz._
  
  def |*| = IntMultiplication.multiplication(value)

  def ordering = if(value < 0) LT else if(value > 0) GT else EQ
}

object IntW {
  implicit def IntTo(n: Int): IntW = new IntW {
    val value = n
  }

  implicit def IntFrom(n: IntW) = n.value
}
