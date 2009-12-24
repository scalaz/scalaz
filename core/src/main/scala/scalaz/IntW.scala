package scalaz

sealed trait IntW extends PimpedType[Int] {
  import Scalaz._

  def ∏ = multiplication(value)

  def ordering = if(value < 0) LT else if(value > 0) GT else EQ
}

trait Ints {
  implicit def IntTo(n: Int): IntW = new IntW {
    val value = n
  }
}
