package scalaz
package std

import scala.concurrent.{ExecutionContext, Future}

trait FutureInstances extends FutureInstances1 {
  implicit def futureMonoid[A](implicit g: Monoid[A], ec: ExecutionContext): Monoid[Future[A]] =
    Monoid.liftMonoid[Future, A]
}
