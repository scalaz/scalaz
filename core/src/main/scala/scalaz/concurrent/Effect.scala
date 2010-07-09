package scalaz
package concurrent

sealed trait Effect[-A] {
  val e: A => Unit
  val strategy: Strategy

  def !(a: A) = strategy(e(a))
}

trait Effects {
  def effect[A](c: A => Unit)(implicit s: Strategy): Effect[A] = new Effect[A] {
    val e = (a: A) => c(a)
    val strategy = s
  }
}

object Effect {
  implicit def EffectFrom[A](e: Effect[A]): A => Unit = e ! _
}
