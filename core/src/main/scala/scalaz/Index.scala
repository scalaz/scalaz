package scalaz

trait Index[F[_]] {
  def index[A](a: F[A]): Int => Option[A]

  def indexOr[A](a: F[A], d: => A): Int => A =
    n => index(a)(n) getOrElse d
}

object Index extends Indexs

trait Indexs {
  implicit def OptionIndex: Index[Option] = new Index[Option] {
    def index[A](a: Option[A]) = i => a filter (_ => i == 0)
  }
}
