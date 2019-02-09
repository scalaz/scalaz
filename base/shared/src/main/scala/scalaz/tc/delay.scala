package scalaz
package tc

/**
 * Given `Delay[A]`, one can suspend execution of an `A` inside of the `A` itself.
 */
trait DelayClass[A] {
  def delay(fa: () => A): A
}

object Delay {
  implicit def function0DelayLazy[A]: Delay[() => A] =
    instanceOf[DelayClass[() => A]](fa => () => fa()())
}
