package scalaz
package wrap

sealed trait LongW {

  import newtypes._
  import LongW._

  val value: Long

  def multiplication: LongMultiplication =
    ^*^.->^*^[Long, LongMultiplication](value)

  def ∏ : LongMultiplication =
    multiplication

  def digits: List[Digit] =
    Digit.digitFromLong(value) match {
      case Some(d) => List(d)
      case None => Digit.digitFromLong(value % 10L).toList ::: ((value / 10L) digits)
    }
}

object LongW extends LongWs

trait LongWs {
  implicit def LongTo(n: Long): LongW = new LongW {
    val value = n
  }
}