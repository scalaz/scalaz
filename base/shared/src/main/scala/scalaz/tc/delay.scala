package scalaz
package tc

import zio.IO

import scala.language.experimental.macros

/**
 * Given `Delay[A]`, one can suspend execution of an `A` inside of the `A` itself.
 */
trait DelayClass[A] {
  def delay(a: () => A): A
  final def d(a: => A): A = delay(() => a)
}

object DelayClass {
  implicit def ioDelay[E, A]: Delay[IO[E, A]] = instanceOf[DelayClass[IO[E, A]]](io => IO.suspend[E, A](io()))
}

trait DelaySyntax {
  implicit final class ToDelayOps[A](a: => A) {
    def d(implicit ev: Delay[A]): A = macro ops.Ops.i_0
  }
}
