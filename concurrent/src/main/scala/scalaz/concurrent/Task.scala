package scalaz.concurrent

import scalaz.Nondeterminism
import scalaz.Traverse
import scalaz.std.list._
import scalaz.std.either._

/* 
 * `Task[A]` is a `Future[Either[Throwable,A]]`, with some convenience 
 * functions for handling exceptions. Its `Monad` instance is derived
 * from the `Monad` instance for `Future`.
 */
class Task[+A](val get: Future[Either[Throwable,A]]) {
  
  def flatMap[B](f: A => Task[B]): Task[B] = 
    new Task(get.flatMap { 
      case Left(e) => Future.now(Left(e))
      case Right(a) => f(a).get
    })

  def map[B](f: A => B): Task[B] = 
    new Task(get map (e => e.right map f))
  
  /* 'Catches' exceptions in the given task and returns them as values. */
  def attempt: Task[Either[Throwable,A]] = 
    new Task(get map { 
      case Left(e) => Right(Left(e))
      case Right(a) => Right(Right(a))
    })

  def onFinish(f: Option[Throwable] => Task[Unit]): Task[A] =
    new Task(get flatMap {
      case Left(e) => f(Some(e)).get *> Future.now(Left(e))
      case r => f(None).get *> Future.now(r)
    })
  
  def handle[B>:A](f: PartialFunction[Throwable,B]): Task[B] = 
    attempt flatMap {
      case Left(e) => f.lift(e) map (Task.now) getOrElse Task.fail(e)
      case Right(a) => Task.now(a) 
    }

  def or[B>:A](t2: Task[B]): Task[B] = 
    new Task(this.get flatMap { 
      case Left(e) => t2.get 
      case a => Future.now(a)
    })
  
  def run: A = get.run match {
    case Left(e) => throw e 
    case Right(a) => a
  }

  def attemptRun: Either[Throwable,A] = 
    try get.run catch { case t: Throwable => Left(t) }

  def runAsync: Future[Either[Throwable,A]] = get.start
}

object Task {
  
  implicit val taskInstance = new Nondeterminism[Task] { 
    val F = Nondeterminism[Future]
    def point[A](a: => A) = new Task(Future.now(Try(a))) 
    def bind[A,B](a: Task[A])(f: A => Task[B]): Task[B] = 
      a flatMap f 
    def chooseAny[A](h: Task[A], t: Seq[Task[A]]): Task[(A, Seq[Task[A]])] =
      new Task ( F.map(F.chooseAny(h.get, t map (_ get))) { case (a, residuals) => 
        a.right.map((_, residuals.map(new Task(_))))
      })
    override def gatherUnordered[A](fs: Seq[Task[A]]): Task[List[A]] = {
      new Task (F.map(F.gatherUnordered(fs.map(_ get)))(eithers => 
        Traverse[List].sequenceU(eithers) 
      ))
    }
  }

  def fail(e: Throwable): Task[Nothing] = new Task(Future.now(Left(e))) 
  def now[A](a: A): Task[A] = new Task(Future.now(Right(a)))
  def suspend[A](a: => Task[A]): Task[A] = new Task(Future.suspend(
    Try(a.get) match { 
      case Left(e) => Future.now(Left(e))
      case Right(f) => f
  }))

  def delay[A](a: => A): Task[A] = suspend(now(a))
  def fork[A](a: => A): Task[A] = new Task(Future(Try(a)).start)
  def async[A](register: (Either[Throwable,A] => Unit) => Unit): Task[A] = 
   new Task(Future.async(register))
  def apply[A](a: => A): Task[A] = fork(a) 

  def Try[A](a: => A): Either[Throwable,A] = 
    try Right(a) catch { case e: Exception => Left(e) }

  import scalaz.syntax.{ApplyOps, ApplicativeOps, FunctorOps, MonadOps}

  implicit def toMonadOps[A](f: Task[A]): MonadOps[Task,A] = 
    taskInstance.monadSyntax.ToMonadOps(f)
  implicit def toApplicativeOps[A](f: Task[A]): ApplicativeOps[Task,A] = 
    taskInstance.applicativeSyntax.ToApplicativeOps(f)
  implicit def toApplyOps[A](f: Task[A]): ApplyOps[Task,A] = 
    taskInstance.applySyntax.ToApplyOps(f)
  implicit def toFunctorOps[A](f: Task[A]): FunctorOps[Task,A] =
    taskInstance.functorSyntax.ToFunctorOps(f)
}
