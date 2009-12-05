package scalaz

sealed trait ShortW {
  val value: Short

  import Scalaz._

  def ∏ = multiplication(value)
}

trait Shorts {
  implicit def ShortTo(n: Short): ShortW = new ShortW {
    val value = n
  }

  implicit def ShortFrom(n: ShortW): Short = n.value
}
