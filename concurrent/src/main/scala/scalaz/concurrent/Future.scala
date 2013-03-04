package scalaz.concurrent

import java.util.concurrent.ExecutorService

import scalaz.Monad
import scalaz.Free.Trampoline
import scalaz.Free
import scalaz.Trampoline
import scalaz.std.function._

/** 
 * A trampolined computation producing an `A` that may include 
 * asynchronous steps. See constructors in companion object.
 */
trait Future[+A] {
  import Future._

  def flatMap[B](f: A => Future[B]): Future[B] = this match {
    case Now(a) => f(a)
    case Suspend(thunk) => BindSuspend(thunk, f)
    case Async(listen) => BindAsync(listen, f)
    case BindSuspend(thunk, g) => 
      Suspend(() => BindSuspend(thunk, g andThen (_ flatMap f)))
    case BindAsync(listen, g) => 
      Suspend(() => BindAsync(listen, g andThen (_ flatMap f))) 
  }

  def map[B](f: A => B): Future[B] =  
    flatMap(f andThen (b => Future.now(b)))

  def listen(cb: A => Trampoline[Unit]): Unit = 
    this.step match {
      case Now(a) => cb(a) 
      case Async(onFinish) => onFinish(cb)
      case BindAsync(onFinish, g) => 
        onFinish(x => Trampoline.delay(g(x)) map (_ listen cb))
    }

  @annotation.tailrec
  final def step: Future[A] = this match {
    case Suspend(thunk) => thunk().step
    case BindSuspend(thunk, f) => (thunk() flatMap f).step 
    case _ => this
  }

  def start: Future[A] = {
    val latch = new java.util.concurrent.CountDownLatch(1)
    @volatile var result: Option[A] = None
    runAsync { a => result = Some(a); latch.countDown } 
    delay { latch.await; result.get }
  }

  def runAsync(cb: A => Unit): Unit = 
    listen(a => Trampoline.done(cb(a)))

  def run: A = {
    val latch = new java.util.concurrent.CountDownLatch(1) 
    @volatile var result: Option[A] = None
    runAsync { a => result = Some(a); latch.countDown }
    latch.await
    result.get
  }
}

object Future {

  case class Now[+A](a: A) extends Future[A]
  case class Async[+A](onFinish: (A => Trampoline[Unit]) => Unit) extends Future[A]
  case class Suspend[+A](thunk: () => Future[A]) extends Future[A]
  case class BindSuspend[A,B](thunk: () => Future[A], f: A => Future[B]) extends Future[B]
  case class BindAsync[A,B](onFinish: (A => Trampoline[Unit]) => Unit,
                            f: A => Future[B]) extends Future[B]
  
  // NB: considered implementing Traverse and Comonad, but these would have
  // to run the Future; leaving out for now

  implicit val futureInstance = new Monad[Future] {
    def bind[A,B](fa: Future[A])(f: A => Future[B]): Future[B] =   
      fa flatMap f
    def point[A](a: => A): Future[A] = now(a)
  }

  def now[A](a: A): Future[A] = Now(a)

  def delay[A](a: => A): Future[A] = Suspend(() => Now(a))

  def fork[A](a: => Future[A]): Future[A] = Future(a) flatMap (a => a)

  def suspend[A](f: => Future[A]): Future[A] = Suspend(() => f)

  def async[A](listen: (A => Unit) => Unit): Future[A] = 
    Async((cb: A => Trampoline[Unit]) => listen { a => cb(a).run })
  
  import java.util.concurrent.{Callable, Executors, ThreadFactory}

  def apply[A](a: => A)(implicit pool: ExecutorService = Strategy.DefaultExecutorService): Future[A] = Async { cb => 
    pool.submit { new Callable[Unit] { def call = cb(a).run }}
  }

  import scalaz.syntax.{ApplyOps, ApplicativeOps, FunctorOps, MonadOps}

  implicit def toMonadOps[A](f: Future[A]): MonadOps[Future,A] = 
    futureInstance.monadSyntax.ToMonadOps(f)
  implicit def toApplicativeOps[A](f: Future[A]): ApplicativeOps[Future,A] = 
    futureInstance.applicativeSyntax.ToApplicativeOps(f)
  implicit def toApplyOps[A](f: Future[A]): ApplyOps[Future,A] = 
    futureInstance.applySyntax.ToApplyOps(f)
  implicit def toFunctorOps[A](f: Future[A]): FunctorOps[Future,A] =
    futureInstance.functorSyntax.ToFunctorOps(f)
}
