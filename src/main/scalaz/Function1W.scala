package scalaz

sealed trait Function1W[T, R] {
  val k: T => R

  def on[X](f: (R, R) => X, t1: T, t2: T) = f(k(t1), k(t2))

  def arrow[A[-_, +_]](implicit a: Arrow[A]) = a arrow k
}

object Function1W {
  def Function1To[T, R](f: T => R) = new Function1W[T, R] {
    val k = f
  }

  def Function1From[T, R](f: Function1W[T, R]) = f.k
}
