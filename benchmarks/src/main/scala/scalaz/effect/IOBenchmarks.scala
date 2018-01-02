// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz.effect

object IOBenchmarks extends RTS {
  import monix.execution.Scheduler

  implicit val monixScheduler: Scheduler = {
    import monix.execution.ExecutionModel.SynchronousExecution
    Scheduler.computation().withExecutionModel(SynchronousExecution)
  }

  class Thunk[A](val unsafePerformIO: () => A) {
    def map[B](ab: A => B): Thunk[B] =
      new Thunk(() => ab(unsafePerformIO()))
    def flatMap[B](afb: A => Thunk[B]): Thunk[B] =
      new Thunk(() => afb(unsafePerformIO()).unsafePerformIO())
    def attempt: Thunk[Either[Throwable, A]] = new Thunk(() => {
      try Right(unsafePerformIO())
      catch {
        case t : Throwable => Left(t)
      }
    })
  }
  object Thunk {
    def apply[A](a: => A): Thunk[A] = new Thunk(() => a)

    def fail[A](t: Throwable): Thunk[A] = new Thunk(() => throw t)
  }
}
