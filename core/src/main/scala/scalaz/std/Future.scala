package scalaz
package std

import _root_.java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{ Try, Success => TSuccess }

trait FutureInstances1 {
  implicit def futureInstance(implicit ec: ExecutionContext): Nondeterminism[Future] with Cobind[Future] with MonadError[Future, Throwable] =
    new FutureInstance

  implicit def futureSemigroup[A](implicit m: Semigroup[A], ec: ExecutionContext): Semigroup[Future[A]] =
    Semigroup.liftSemigroup[Future, A]
}

private class FutureInstance(implicit ec: ExecutionContext) extends Nondeterminism[Future] with Cobind[Future] with MonadError[Future, Throwable] {
  def point[A](a: => A): Future[A] = Future(a)
  def bind[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa flatMap f
  override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa map f
  def cobind[A, B](fa: Future[A])(f: Future[A] => B): Future[B] = Future(f(fa))
  override def cojoin[A](a: Future[A]): Future[Future[A]] = Future.successful(a)

  def chooseAny[A](head: Future[A], tail: IList[Future[A]]): Future[(A, IList[Future[A]])] = {
    val fs = (head +: tail).zipWithIndex
    val counter = new AtomicInteger(fs.length)
    val result = Promise[(A, Int)]()
    var mutableResult = result
    def attemptComplete(t: Try[(A, Int)]): Unit = {
      val remaining = counter.decrementAndGet
      val result = mutableResult
      if (result != null) {
        t match {
          case TSuccess(_) =>
            val _ = result tryComplete t
            mutableResult = null
          case _ if remaining == 0 =>
            val _ = result tryComplete t
            mutableResult = null
          case _ =>
        }
      }
    }

    fs map { case (fa, i) =>
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

  override def gather[A](fs: IList[Future[A]]): Future[IList[A]] =
    sequence(fs)

  // override for actual parallel execution
  override def ap[A, B](fa: => Future[A])(fab: => Future[A => B]) =
    fab zip fa map { case (fa, a) => fa(a) }

  def raiseError[A](e: Throwable): Future[A] =
    Future.failed(e)

  def handleError[A](fa: Future[A])(f: Throwable => Future[A]): Future[A] =
    fa.recoverWith { case e => f(e) }
}

object scalaFuture extends FutureInstances
