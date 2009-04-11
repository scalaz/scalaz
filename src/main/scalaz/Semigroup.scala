package scalaz

sealed trait Semigroup[S] {
  def append(s1: S, s2: => S): S
}

object Semigroup {
  def semigroup[S](f: (S, => S) => S) = new Semigroup[S] {
    def append(s1: S, s2: => S) = f(s1, s2)
  }

  implicit val StringSemigroup = semigroup[String](_ + _)
}
