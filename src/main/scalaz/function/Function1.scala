package scalaz.function

sealed trait Function1[-T, +R] {
  val f: T => R

  def map[B](g: R => B) = f andThen g

  import Function1._

  def flatMap[B, TT <: T](g: R => Function1[TT, B]) =
    (t: TT) => g(Function1.this.f(t)).f(t)
}

object Function1 {
  implicit def ScalaFunction1Function1[T, R](ff: T => R) = new Function1[T, R] {
    val f = ff
  }

  implicit def Function1ScalaFunction1[T, R](f: Function1[T, R]) = f.f
}
