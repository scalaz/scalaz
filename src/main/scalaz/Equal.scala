package scalaz

sealed trait Equal[-A] {
  def equal(a1: A, a2: A): Boolean
}

object Equal {
  def equal[A](f: (A, A) => Boolean): Equal[A] = new Equal[A] {
    def equal(a1: A, a2: A) = f(a1, a2)
  }

  implicit val StringEqual = equal[String](_ == _)
}
