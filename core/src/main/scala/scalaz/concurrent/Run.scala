package scalaz
package concurrent

sealed trait Run[A] {
  val apply: A => Unit
  val strategy: Strategy

  def !(a: A) = strategy(apply(a))
}

object Run extends Runs

trait Runs {
  def effect[A](c: A => Unit)(implicit s: Strategy): Run[A] = new Run[A] {
    val apply = (a: A) => c(a)
    val strategy = s
  }

  implicit def EffectFrom[A](e: Run[A]): A => Unit =
    e ! _
}
