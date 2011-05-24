package scalaz
package wrap

sealed trait CharW {

  import data._, Digit._, Alpha._
  import newtypes._

  val value: Char

  def multiplication: CharMultiplication =
    ^*^.->^*^[Char, CharMultiplication](value)

  def ∏ : CharMultiplication =
    multiplication

  def digit: Option[Digit] =
    digits find (_.toChar == value)

  def alpha: Option[Alpha] =
    alphas find (_.toChar == value)
}

object CharW extends CharWs

trait CharWs {
  implicit def CharTo(n: Char): CharW = new CharW {
    val value = n
  }
}