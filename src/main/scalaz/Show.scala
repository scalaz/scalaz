package scalaz

trait Show[-A] {
  def show(a: A): List[Char]

  def shows(a: A) = show(a).mkString
}

object Show {
  def show[A](f: A => List[Char]) = new Show[A] {
    def show(a: A) = f(a)
  }

  def shows[A](f: A => String) = show[A](f(_).toList)

  def showA[A] = show[A](_.toString.toList)

  implicit def StringShow = showA[String]
}
