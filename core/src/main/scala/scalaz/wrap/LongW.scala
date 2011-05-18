package scalaz
package wrap

sealed trait LongW {

  import newtypes._

  val value: Long

  def multiplication: LongMultiplication =
    Pack.pack[Long, LongMultiplication](value)

  def ∏ : LongMultiplication =
    multiplication
}

trait LongWs {
  implicit def LongTo(n: Long): LongW = new LongW {
    val value = n
  }
}