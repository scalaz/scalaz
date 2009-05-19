package scalaz

sealed trait Continuation[R, +A] {
  def apply(f: A => R): R

  import Scalaz._

  def using[AA >: A, B](f: (B => R) => AA => R) = {
    continuation[R, B](f andThen apply)
  }
}

object Continuation {
  def continuation[R, A](f: (A => R) => R) = new Continuation[R, A] {
    def apply(k: A => R) = f(k)
  }

  trait ContinuationConstant[A] {
    def apply[R](r: => R): Continuation[R, A]
  }

  def constant[A] = new ContinuationConstant[A] {
    def apply[R](r: => R) = continuation[R, A](_ => r)
  }
}
