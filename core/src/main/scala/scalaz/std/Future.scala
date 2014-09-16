package scalaz
package std

import scala.concurrent.{Await, CanAwait, ExecutionContext, Future}
import scala.concurrent.duration.Duration

import scalaFuture.newFutureInstance

trait FutureInstances1 {
  // for binary compatibility
  def futureInstance(implicit ec: ExecutionContext): Monad[Future] with Cobind[Future] =
    new FutureInstance

  implicit def futureSemigroup[A](implicit m: Semigroup[A], ec: ExecutionContext): Semigroup[Future[A]] =
    Semigroup.liftSemigroup[Future, A]
}

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

private[scalaz] class FutureInstance(implicit ec: ExecutionContext) extends Monad[Future] with Cobind[Future] with MonadError[({type λ[α,β] = Future[β]})#λ, Throwable] with Catchable[Future] {
  def point[A](a: => A): Future[A] = Future(a)
  def bind[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa flatMap f
  override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa map f
  def cobind[A, B](fa: Future[A])(f: Future[A] => B): Future[B] = Future(f(fa))
  override def cojoin[A](a: Future[A]): Future[Future[A]] = Future(a)

  // override for actual parallel execution
  override def ap[A, B](fa: => Future[A])(fab: => Future[A => B]) = {
    val fa0 = join(Future(fa))
    fa0 zip fab map { case (a, fa) => fa(a) }
  }

  def attempt[A](f: Future[A]): Future[Throwable \/ A] =
    f.map(\/.right).recover { case e => -\/(e) }

  def fail[A](e: Throwable): Future[A] =
    Future.failed(e)

  def raiseError[A](e: Throwable): Future[A] =
    fail(e)

  def handleError[A](fa: Future[A])(f: Throwable => Future[A]): Future[A] =
    fa.recoverWith{ case e: Throwable => f(e) }
}

object scalaFuture extends FutureInstances {
  implicit def newFutureInstance(implicit ec: ExecutionContext): Cobind[Future] with MonadError[({type λ[α,β] = Future[β]})#λ, Throwable] with Catchable[Future] =
    new FutureInstance
}
