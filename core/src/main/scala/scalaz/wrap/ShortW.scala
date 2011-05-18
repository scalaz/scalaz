package scalaz

sealed trait ShortW extends PimpedType[Short] {
  val value: Short

  import Scalaz._

  def ∏ : ShortMultiplication = multiplication(value)
}

trait Shorts {
  implicit def ShortTo(n: Short): ShortW = new ShortW {
    val value = n
  }
}
