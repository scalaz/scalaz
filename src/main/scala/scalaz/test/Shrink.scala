package scalaz.test

sealed trait Shrink[A] {
  def shrink(a: A): Stream[A]
}

object Shrink {
  def shrink[A](f: A => Stream[A]) = new Shrink[A] {
    def shrink(a: A) = f(a)
  }
}