package scalaz
package std

import _root_.java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{ Try, Success => TSuccess }

trait FutureInstances1 {
  implicit def futureInstance(implicit ec: ExecutionContext): Nondeterminism[Future] with Cobind[Future] with MonadError[Future, Throwable] with Catchable[Future] =
    new FutureInstance

  implicit def futureSemigroup[A](implicit m: Semigroup[A], ec: ExecutionContext): Semigroup[Future[A]] =
    Semigroup.liftSemigroup[Future, A]
}

private class FutureInstance(implicit ec: ExecutionContext) extends Nondeterminism[Future] with Cobind[Future] with MonadError[Future, Throwable] with Catchable[Future] {
  def point[A](a: => A): Future[A] = Future(a)
  def bind[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa flatMap f
  override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa map f
  def cobind[A, B](fa: Future[A])(f: Future[A] => B): Future[B] = Future(f(fa))
  override def cojoin[A](a: Future[A]): Future[Future[A]] = Future.successful(a)

  def chooseAny[A](head: Future[A], tail: Seq[Future[A]]): Future[(A, Seq[Future[A]])] = {
    val fs = (head +: tail).iterator.zipWithIndex.toIndexedSeq
    val counter = new AtomicInteger(fs.size)
    val result = Promise[(A, Int)]()
    def attemptComplete(t: Try[(A, Int)]): Unit = {
      val remaining = counter.decrementAndGet
      t match {
        case TSuccess(_) => result tryComplete t
        case _ if remaining == 0 => result tryComplete t
        case _ =>
      }
    }

    fs foreach { case (fa, i) =>
      fa.onComplete { t => attemptComplete(t.map(_ -> i)) }
    }

    result.future.map { case (a, i) =>
      (a, fs.collect { case (fa, j) if j != i => fa })
    }
  }

  override def mapBoth[A,B,C](a: Future[A], b: Future[B])(f: (A,B) => C): Future[C] =
    (a zip b).map(f.tupled)

  override def both[A,B](a: Future[A], b: Future[B]): Future[(A,B)] =
    a zip b

  override def gather[A](fs: Seq[Future[A]]): Future[List[A]] =
    Future.sequence(fs.toList)

  // override for actual parallel execution
  override def ap[A, B](fa: => Future[A])(fab: => Future[A => B]) =
    fab zip fa map { case (fa, a) => fa(a) }

  def attempt[A](f: Future[A]): Future[Throwable \/ A] =
    f.map(\/.right).recover { case e => -\/(e) }

  def fail[A](e: Throwable): Future[A] =
    Future.failed(e)

  def raiseError[A](e: Throwable): Future[A] =
    fail(e)

  def handleError[A](fa: Future[A])(f: Throwable => Future[A]): Future[A] =
    fa.recoverWith{case t => f(t)}
}

object scalaFuture extends FutureInstances
