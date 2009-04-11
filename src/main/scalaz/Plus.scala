package scalaz

trait Plus[P[_]] {
  def plus[A](a1: P[A], a2: => P[A]): P[A]
}

object Plus {
  implicit val OptionPlus = new Plus[Option] {
    def plus[A](a1: Option[A], a2: => Option[A]) = a1 orElse a2
  }
}
