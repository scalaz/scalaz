package scalaz

sealed trait CharW extends PimpedType[Char] {
  import Scalaz._
  
  def ∏ = multiplication(value)

  def digit = digits find (_.toChar == value)

  def alpha = alphas find(_.toChar == value) 
}

trait Chars {
  implicit def CharTo(c: Char): CharW = new CharW {
    val value = c
  }
}
