package scalaz
package std.effect

import scalaz.effect.IO

import scala.concurrent.{Future, ExecutionContext}

trait FutureFunctions {
  def forkIO[A](a: IO[A])(implicit ec: ExecutionContext): IO[Future[A]] =
    IO(Future(a.unsafePerformIO))
}

object scalaFuture extends FutureFunctions
