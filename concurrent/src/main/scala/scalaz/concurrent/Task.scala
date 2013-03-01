package scalaz.concurrent

import scalaz.Monad

/* 
 * `Task[A]` is a `Future[Either[Throwable,A]]`, with some convenience 
 * functions for handling exceptions. Its `Monad` instance is derived
 * from the `Monad` instance for `Future`.
 */
case class Task[+A](get: Future[Either[Throwable,A]]) {
  
  def flatMap[B](f: A => Task[B]): Task[B] = 
    Task(get.flatMap { 
      case Left(e) => Future.now(Left(e))
      case Right(a) => f(a).get
    })

  def map[B](f: A => B): Task[B] = 
    Task(get map (e => e.right map f))
  
  /* 'Catches' exceptions in the given task and returns them as values. */
  def attempt: Task[Either[Throwable,A]] = 
    Task(get map { 
      case Left(e) => Right(Left(e))
      case Right(a) => Right(Right(a))
    })

  def onFinish(f: Option[Throwable] => Task[Unit]): Task[A] =
    Task(get flatMap {
      case Left(e) => f(Some(e)).get *> Future.now(Left(e))
      case r => f(None).get *> Future.now(r)
    })
  
  def handle[B>:A](f: PartialFunction[Throwable,B]): Task[B] = 
    attempt flatMap {
      case Left(e) => f.lift(e) map (Task.now) getOrElse Task.fail(e)
      case Right(a) => Task.now(a) 
    }

  def or[B>:A](t2: Task[B]): Task[B] = 
    Task(this.get flatMap { 
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
  
  val taskInstance = new Monad[Task] { 
    def point[A](a: => A) = Task(Future.now(Try(a))) 
    def bind[A,B](a: Task[A])(f: A => Task[B]): Task[B] = 
      a flatMap f 
  }

  def fail(e: Throwable): Task[Nothing] = Task(Future.now(Left(e))) 
  def now[A](a: A): Task[A] = Task(Future.now(Right(a)))
  def suspend[A](a: => Task[A]): Task[A] = Task(Future.suspend(
    Try(a.get) match { 
      case Left(e) => Future.now(Left(e))
      case Right(f) => f
  }))

  def delay[A](a: => A): Task[A] = suspend(now(a))
  def fork[A](a: => A): Task[A] = Task(Future(Try(a)).start)
  def async[A](register: (Either[Throwable,A] => Unit) => Unit): Task[A] = 
   Task(Future.async(register))

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
