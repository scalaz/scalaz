package scalaz
package std

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

trait FutureInstances extends FutureInstances1 {
  /**
   * Requires explicit usage as the use of `Await.result`. Can throw an exception, which is inherently bad.
   */
  def futureComonad(duration: Duration)(implicit executionContext: ExecutionContext): Comonad[Future] = new FutureInstance with Comonad[Future] {
    def copoint[A](f: Future[A]): A = Await.result(f, duration)
  }

  implicit def futureMonoid[A](implicit g: Monoid[A], ec: ExecutionContext): Monoid[Future[A]] =
    Monoid.liftMonoid[Future, A]
}
