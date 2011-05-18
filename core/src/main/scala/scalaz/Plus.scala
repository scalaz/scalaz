package scalaz

trait Plus[F[_]] {
  def plus[A](a1: F[A], a2: => F[A]): F[A]
}

object Plus extends Pluss

trait Pluss {
  implicit def OptionPlus: Plus[Option] = new Plus[Option] {
    def plus[A](a1: Option[A], a2: => Option[A]) =
      a1 orElse a2
  }

  implicit def ListPlus: Plus[List] = new Plus[List] {
    def plus[A](a1: List[A], a2: => List[A]) =
      a1 ::: a2
  }

  implicit def StreamPlus: Plus[Stream] = new Plus[Stream] {
    def plus[A](a1: Stream[A], a2: => Stream[A]) =
      a1 #::: a2
  }
}