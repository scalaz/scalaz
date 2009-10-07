package scalaz.concurrent

sealed trait Effect[-A] extends (A => Unit) {
  val e: A => () => Unit
  val strategy: Strategy[Unit]

  def !(a: A) = strategy(e(a))

  def apply(a: A) = this ! a
}

object Effect {
  def effect[A](c: A => Unit)(implicit s: Strategy[Unit]) = new Effect[A] {
    val e = (a: A) => () => c(a)
    val strategy = s
  }
}