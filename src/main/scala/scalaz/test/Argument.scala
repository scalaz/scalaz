package scalaz.test

// todo try to be rid of this type variable i.e. use display and remove 'value' member
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