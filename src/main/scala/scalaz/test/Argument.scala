package scalaz.test

sealed trait Argument[+A] {
  val value: A
  val shrinks: Int
  val display: String
}

object Argument {
  import S._
  
  def argument[A](a: A, s: Int)(implicit sh: Show[A]) = new Argument[A] {
    val value = a
    val shrinks = s
    val display = a.shows
  }
}