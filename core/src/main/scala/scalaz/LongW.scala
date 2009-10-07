package scalaz

sealed trait LongW {
  val value: Long

  def digits: List[Digit] = {
    import Digit._
    import LongW._

    if(value == 0) List(0L)
    else if(value < 10) List(value)
    else value % 10L :: (value / 10L digits)
  }

  def |*| = LongMultiplication.multiplication(value)  
}

object LongW {
  implicit def LongTo(n: Long): LongW = new LongW {
    val value = n
  }

  implicit def LongFrom(n: LongW) = n.value
}
