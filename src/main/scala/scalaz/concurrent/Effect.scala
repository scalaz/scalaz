package scalaz.concurrent

sealed trait Effect[-A] {
  val e: A => () => Unit
  val strategy: Strategy[Unit]

  def !(a: A) = strategy(e(a))
}

object Effect {
  def effect[A](c: A => Unit)(implicit s: Strategy[Unit]) = new Effect[A] {
    val e = (a: A) => () => c(a)
    val strategy = s
  }

  implicit def effectFrom[A](implicit a: Effect[A]): A => Unit = (a ! _)
}