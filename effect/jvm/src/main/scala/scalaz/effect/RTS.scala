// Copyright (C) 2017-2018 John A. De Goes. All rights reserved.
package scalaz
package effect

import scala.{ ::, Any, AnyRef, Array, List, Nil, None, Nothing, Option, Some }
import scala.annotation.{ switch, tailrec }
import scala.concurrent.duration.Duration

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ Executors, TimeUnit }
import java.lang.{ Math, Runnable, Runtime }

/**
 * This trait provides a high-performance implementation of a runtime system for
 * the `IO` monad on the JVM.
 */
trait RTS {
  import RTS._

  /**
   * Effectfully and synchronously interprets an `IO[E, A]`, either throwing an
   * error, running forever, or producing an `A`.
   */
  final def unsafePerformIO[E, A](io: IO[E, A]): A = tryUnsafePerformIO(io) match {
    case ExitResult.Completed(v)  => v
    case ExitResult.Terminated(t) => throw t
    case ExitResult.Failed(e)     => throw Errors.UnhandledError(e)
  }

  final def unsafePerformIOAsync[E, A](io: IO[E, A])(k: ExitResult[E, A] => Unit): Unit = {
    val context = new FiberContext[E, A](this, defaultHandler)

    context.evaluate(io)

    context.register(k) match {
      case Async.Now(v) => k(v)
      case _            =>
    }
  }

  /**
   * Effectfully interprets an `IO`, blocking if necessary to obtain the result.
   */
  final def tryUnsafePerformIO[E, A](io: IO[E, A]): ExitResult[E, A] = {
    val result = new AtomicReference[ExitResult[E, A]](null)

    val context = new FiberContext[E, A](this, defaultHandler)

    context.evaluate(io)

    (context.register { (r: ExitResult[E, A]) =>
      result.synchronized {
        result.set(r)

        result.notifyAll()
      }
    }) match {
      case Async.Now(v) =>
        result.set(v)

      case _ =>
        while (result.get eq null) {
          result.synchronized {
            if (result.get eq null) result.wait()
          }
        }
    }

    result.get
  }

  final def unsafeShutdownAndWait(timeout: Duration): Unit = {
    threadPool.shutdown()
    threadPool.awaitTermination(timeout.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  /**
   * The default handler for unhandled exceptions in the main fiber, and any
   * fibers it forks that recursively inherit the handler.
   */
  def defaultHandler[E]: Throwable => IO[E, Unit] =
    (t: Throwable) => IO.sync(t.printStackTrace())

  /**
   * The main thread pool used for executing fibers.
   */
  val threadPool = Executors.newFixedThreadPool(Math.max(Runtime.getRuntime().availableProcessors(), 2))

  /**
   * This determines the maximum number of resumptions placed on the stack
   * before a fiber is shifted over to a new thread to prevent stack overflow.
   */
  val MaxResumptionDepth = 10

  /**
   * Determines the maximum number of operations executed by a fiber before
   * yielding to other fibers.
   *
   * FIXME: Replace this entirely with the new scheme.
   */
  final val YieldMaxOpCount = 1048576

  lazy val scheduledExecutor = Executors.newScheduledThreadPool(1)

  final def submit[A](block: => A): Unit = {
    threadPool.submit(new Runnable {
      def run: Unit = { block; () }
    })

    ()
  }

  final def schedule[E, A](block: => A, duration: Duration): Async[E, Unit] =
    if (duration == Duration.Zero) {
      submit(block)

      Async.later[E, Unit]
    } else {
      val future = scheduledExecutor.schedule(new Runnable {
        def run: Unit = submit(block)
      }, duration.toNanos, TimeUnit.NANOSECONDS)

      Async.maybeLater { (t: Throwable) =>
        future.cancel(true); ()
      }
    }

  final def impureCanceler(canceler: PureCanceler): Canceler =
    th => unsafePerformIO(canceler(th))
}

private object RTS {
  // Utility function to avoid catching truly fatal exceptions. Do not allocate
  // memory here since this would defeat the point of checking for OOME.
  def nonFatal(t: Throwable): Boolean =
    !t.isInstanceOf[InternalError] && !t.isInstanceOf[OutOfMemoryError]

  sealed trait RaceState
  object RaceState {
    case object Started  extends RaceState
    case object Finished extends RaceState
  }

  type Callback[E, A] = ExitResult[E, A] => Unit

  @inline
  final def nextInstr[E](value: Any, stack: Stack): IO[E, Any] =
    if (!stack.isEmpty) stack.pop()(value).asInstanceOf[IO[E, Any]] else null

  object Catcher extends Function[Any, IO[Any, Any]] {
    final def apply(v: Any): IO[Any, Any] = IO.now(\/-(v))
  }

  object IdentityCont extends Function[Any, IO[Any, Any]] {
    final def apply(v: Any): IO[Any, Any] = IO.now(v)
  }

  final case class Finalizer[E](finalizer: ExitResult[E, Any] => IO[Void, Unit]) extends Function[Any, IO[E, Any]] {
    final def apply(v: Any): IO[E, Any] = IO.now(v)
  }

  final class Stack() {
    type Cont = Any => IO[_, Any]

    private[this] var array   = new Array[AnyRef](13)
    private[this] var size    = 0
    private[this] var nesting = 0

    def isEmpty: Boolean = size == 0

    def push(a: Cont): Unit =
      if (size == 13) {
        array = Array(array, a, null, null, null, null, null, null, null, null, null, null, null)
        size = 2
        nesting += 1
      } else {
        array(size) = a
        size += 1
      }

    def pop(): Cont = {
      val idx = size - 1
      var a   = array(idx)
      if (idx == 0 && nesting > 0) {
        array = a.asInstanceOf[Array[AnyRef]]
        a = array(12)
        array(12) = null // GC
        size = 12
        nesting -= 1
      } else {
        array(idx) = null // GC
        size = idx
      }
      a.asInstanceOf[Cont]
    }
  }

  /**
   * An implementation of Fiber that maintains context necessary for evaluation.
   */
  final class FiberContext[E, A](rts: RTS, val unhandled: Throwable => IO[Void, Unit]) extends Fiber[E, A] {
    import FiberStatus._
    import java.util.{ Collections, Set, WeakHashMap }
    import rts.{ MaxResumptionDepth, YieldMaxOpCount }

    // Accessed from multiple threads:
    private[this] val status = new AtomicReference[FiberStatus[E, A]](FiberStatus.Initial[E, A])
    //@volatile
    private[this] var killed = false

    // TODO: A lot can be pulled out of status to increase performance
    // Also the size of this structure should be minimized with laziness used
    // to optimize further, to make forking a cheaper operation.

    // Accessed from within a single thread (not necessarily the same):
    private[this] var noInterrupt                               = 0
    private[this] var supervised: List[Set[FiberContext[_, _]]] = Nil
    private[this] var supervising                               = 0

    private[this] val stack: Stack = new Stack()

    final def collectDefect[E, A](e: ExitResult[E, A]): List[Throwable] =
      e match {
        case ExitResult.Terminated(t) => t :: Nil
        case ExitResult.Completed(_)  => Nil
        case ExitResult.Failed(_)     => Nil
      }

    /**
     * Creates an action to dispatch a list of errors to the fiber's uncaught
     * error handler.
     *
     * @param errors  The effectfully produced list of errors, in reverse order.
     */
    final def dispatchErrors(errors: IO[Void, List[Throwable]]): IO[Void, Unit] =
      errors.flatMap(
        // Each error produced by a finalizer must be handled using the
        // context's unhandled exception handler:
        _.reverse.map(unhandled).foldLeft(IO.unit[Void])(_ *> _)
      )

    /**
     * Catches an exception, returning a (possibly null) finalizer action that
     * must be executed. It is painstakingly *guaranteed* that the stack will be
     * empty in the sole case the exception was not caught by any exception
     * handler—i.e. the exceptional case.
     *
     * @param err   The exception that is being thrown.
     */
    final def catchError[E2](err: E): IO[E2, List[Throwable]] = {
      var finalizer: IO[E2, List[Throwable]] = null
      var body: ExitResult[E, Any]           = null

      var caught = false

      // Unwind the stack, looking for exception handlers and coalescing
      // finalizers.
      while (!caught && !stack.isEmpty) {
        stack.pop() match {
          case `Catcher` => caught = true
          case f0: Finalizer[_] =>
            val f = f0.asInstanceOf[Finalizer[E]]

            // Lazy initialization of body:
            if (body eq null) body = ExitResult.Failed(err)

            val currentFinalizer: IO[E2, List[Throwable]] = f.finalizer(body).run.map(collectDefect)

            if (finalizer eq null) finalizer = currentFinalizer
            else finalizer = finalizer.zipWith(currentFinalizer)(_ ++ _)
          case _ =>
        }
      }

      // This may seem strange but we need to maintain the invariant that an
      // empty stack means the exception was *not* caught. So if the exception
      // was caught but the stack is empty, we make the stack non-empty. This
      // lets us return only the finalizer, which will be null for common cases,
      // and result in zero heap allocations for the happy path.
      if (caught && stack.isEmpty) stack.push(IdentityCont)

      finalizer
    }

    /**
     * Empties the stack, collecting all finalizers and coalescing them into an
     * action that produces a list (possibly empty) of errors during finalization.
     *
     * @param error The error being used to interrupt the fiber.
     */
    final def interruptStack[E2](error: Throwable): IO[E2, List[Throwable]] = {
      // Use null to achieve zero allocs for the common case of no finalizers:
      var finalizer: IO[E2, List[Throwable]] = null

      if (!stack.isEmpty) {
        // Any finalizers will require ExitResult. Here we fake lazy evaluation
        // to eliminate unnecessary allocation:
        var body: ExitResult[E, Any] = null

        while (!stack.isEmpty) {
          // Peel off all the finalizers, composing them into a single finalizer
          // that produces a possibly empty list of errors that occurred when
          // executing the finalizers. The order of errors is outer-to-inner
          // (reverse chronological).
          stack.pop() match {
            case f0: Finalizer[_] =>
              val f = f0.asInstanceOf[Finalizer[E]]

              // Lazy initialization of body:
              if (body eq null) body = ExitResult.Terminated(error)

              val currentFinalizer = f.finalizer(body).run[E2].map(collectDefect)

              if (finalizer eq null) finalizer = currentFinalizer
              else finalizer = finalizer.zipWith(currentFinalizer)(_ ++ _)
            case _ =>
          }
        }
      }

      finalizer
    }

    /**
     * The main interpreter loop for `IO` actions. For purely synchronous actions,
     * this will run to completion unless required to yield to other fibers.
     * For mixed actions, the loop will proceed no further than the first
     * asynchronous boundary.
     *
     * @param io0 The `IO` to evaluate on the fiber.
     */
    final def evaluate(io0: IO[E, _]): Unit = {
      // Do NOT accidentally capture any of local variables in a closure,
      // or Scala will wrap them in ObjectRef and performance will plummet.
      var curIo: IO[E, Any] = io0.asInstanceOf[IO[E, Any]]

      while (curIo ne null) {
        try {
          // Put the maximum operation count on the stack for fast access:
          val maxopcount = YieldMaxOpCount

          var result: ExitResult[E, Any] = null
          var eval: Boolean              = true
          var opcount: Int               = 0

          do {
            // Check to see if the fiber should continue executing or not:
            val die = shouldDie

            if (die eq None) {
              // Fiber does not need to be interrupted, but might need to yield:
              if (opcount == maxopcount) {
                // Cooperatively yield to other fibers currently suspended.
                // FIXME: Replace with the new design.
                eval = false

                opcount = 0

                // Cannot capture `curIo` since it will be boxed into `ObjectRef`,
                // which destroys performance, so we create a temp val here.
                val tmpIo = curIo

                rts.submit(evaluate(tmpIo))
              } else {
                // Fiber is neither being interrupted nor needs to yield. Execute
                // the next instruction in the program:
                (curIo.tag: @switch) match {
                  case IO.Tags.FlatMap =>
                    val io = curIo.asInstanceOf[IO.FlatMap[E, Any, Any]]

                    val nested = io.io

                    // A mini interpreter for the left side of FlatMap that evaluates
                    // anything that is 1-hop away. This eliminates heap usage for the
                    // happy path.
                    (nested.tag: @switch) match {
                      case IO.Tags.Point =>
                        val io2 = nested.asInstanceOf[IO.Point[E, Any]]

                        curIo = io.flatMapper(io2.value())

                      case IO.Tags.Strict =>
                        val io2 = nested.asInstanceOf[IO.Strict[E, Any]]

                        curIo = io.flatMapper(io2.value)

                      case IO.Tags.SyncEffect =>
                        val io2 = nested.asInstanceOf[IO.SyncEffect[E, Any]]

                        curIo = io.flatMapper(io2.effect())

                      case _ =>
                        // Fallback case. We couldn't evaluate the LHS so we have to
                        // use the stack:
                        curIo = nested

                        stack.push(io.flatMapper)
                    }

                  case IO.Tags.Point =>
                    val io = curIo.asInstanceOf[IO.Point[E, Any]]

                    val value = io.value()

                    curIo = nextInstr[E](value, stack)

                    if (curIo eq null) {
                      eval = false
                      result = ExitResult.Completed(value)
                    }

                  case IO.Tags.Strict =>
                    val io = curIo.asInstanceOf[IO.Strict[E, Any]]

                    val value = io.value

                    curIo = nextInstr[E](value, stack)

                    if (curIo eq null) {
                      eval = false
                      result = ExitResult.Completed(value)
                    }

                  case IO.Tags.SyncEffect =>
                    val io = curIo.asInstanceOf[IO.SyncEffect[E, Any]]

                    val value = io.effect()

                    curIo = nextInstr[E](value, stack)

                    if (curIo eq null) {
                      eval = false
                      result = ExitResult.Completed(value)
                    }

                  case IO.Tags.Fail =>
                    val io = curIo.asInstanceOf[IO.Fail[E, Any]]

                    val error = io.error

                    val finalizer = catchError[Void](error)

                    if (stack.isEmpty) {
                      // Error not caught, stack is empty:
                      if (finalizer eq null) {
                        // No finalizer, so immediately produce the error.
                        eval = false
                        result = ExitResult.Failed(error)

                        // Report the uncaught error to the supervisor:
                        rts.submit(rts.unsafePerformIO(unhandled(Errors.UnhandledError(error))))
                      } else {
                        // We have finalizers to run. We'll resume executing with the
                        // uncaught failure after we have executed all the finalizers:
                        val finalization = dispatchErrors(finalizer)
                        val completer    = io

                        // Do not interrupt finalization:
                        this.noInterrupt += 1

                        curIo = ensuringUninterruptibleExit(finalization[E] *> completer)
                      }
                    } else {
                      // Error caught:
                      val value = -\/(error)

                      if (finalizer eq null) {
                        // No finalizer to run:
                        curIo = nextInstr[E](value, stack)

                        if (curIo eq null) {
                          eval = false
                          result = ExitResult.Completed(value)
                        }
                      } else {
                        // Must run finalizer first:
                        val finalization = dispatchErrors(finalizer)
                        val completer    = IO.now[E, Any](value)

                        // Do not interrupt finalization:
                        this.noInterrupt += 1

                        curIo = ensuringUninterruptibleExit(finalization[E] *> completer)
                      }
                    }

                  case IO.Tags.AsyncEffect =>
                    val io = curIo.asInstanceOf[IO.AsyncEffect[E, Any]]

                    val id = enterAsyncStart()

                    try {
                      io.register(resumeAsync) match {
                        case Async.Now(value) =>
                          // Value returned synchronously, callback will never be
                          // invoked. Attempt resumption now:
                          if (shouldResumeAsync()) {
                            value match {
                              case ExitResult.Completed(v) =>
                                curIo = nextInstr[E](v, stack)

                                if (curIo eq null) {
                                  eval = false
                                  result = value
                                }
                              case ExitResult.Terminated(t) =>
                                curIo = IO.terminate(t)
                              case ExitResult.Failed(e) =>
                                curIo = IO.fail(e)
                            }
                          } else {
                            // Completion handled by interruptor:
                            eval = false
                          }

                        case Async.MaybeLater(canceler) =>
                          // We have a canceler, attempt to store a reference to
                          // it in case the async computation is interrupted:
                          awaitAsync(id, canceler)

                          eval = false

                        case Async.MaybeLaterIO(pureCancel) =>
                          // As for the case above this stores an impure canceler
                          // obtained performing the pure canceler on the same thread
                          awaitAsync(id, rts.impureCanceler(pureCancel))

                          eval = false
                      }
                    } finally enterAsyncEnd()

                  case IO.Tags.AsyncIOEffect =>
                    val io = curIo.asInstanceOf[IO.AsyncIOEffect[E, Any]]

                    enterAsyncStart()

                    try {
                      val value = rts.tryUnsafePerformIO(io.register(resumeAsync))

                      // Value returned synchronously, callback will never be
                      // invoked. Attempt resumption now:
                      if (shouldResumeAsync()) {
                        value match {
                          case ExitResult.Completed(v) =>
                            curIo = nextInstr[E](v, stack)

                            if (curIo eq null) {
                              eval = false
                              result = value.asInstanceOf[ExitResult[E, Any]]
                            }
                          case ExitResult.Terminated(t) =>
                            curIo = IO.terminate(t)
                          case ExitResult.Failed(e) =>
                            curIo = IO.fail(e)
                        }
                      } else {
                        // Completion handled by interruptor:
                        eval = false
                      }
                    } finally enterAsyncEnd()

                  case IO.Tags.Attempt =>
                    val io = curIo.asInstanceOf[IO.Attempt[E, Any, Any]]

                    curIo = io.value

                    stack.push(Catcher)

                  case IO.Tags.Fork =>
                    val io = curIo.asInstanceOf[IO.Fork[_, E, Any]]

                    val optHandler = Maybe.toOption(io.handler)

                    val handler = if (optHandler eq None) unhandled else optHandler.get

                    val value: FiberContext[_, Any] = fork(io.value, handler)

                    supervise(value)

                    curIo = nextInstr[E](value, stack)

                    if (curIo eq null) {
                      eval = false
                      result = ExitResult.Completed(value)
                    }

                  case IO.Tags.Race =>
                    val io = curIo.asInstanceOf[IO.Race[E, Any, Any, Any]]

                    curIo = raceWith(unhandled, io.left, io.right, io.finishLeft, io.finishRight)

                  case IO.Tags.Suspend =>
                    val io = curIo.asInstanceOf[IO.Suspend[E, Any]]

                    curIo = io.value()

                  case IO.Tags.Bracket =>
                    val io = curIo.asInstanceOf[IO.Bracket[E, Any, Any]]

                    val ref = new AtomicReference[Any]()

                    val finalizer =
                      Finalizer[E](rez => IO.suspend(if (null != ref.get) io.release(rez, ref.get) else IO.unit))

                    stack.push(finalizer)

                    // TODO: This is very heavyweight and could benefit from
                    // optimization.
                    curIo = for {
                      a <- (for {
                            a <- io.acquire
                            _ <- IO.sync(ref.set(a))
                          } yield a).uninterruptibly
                      b <- io.use(a)
                      _ <- (io.release(ExitResult.Completed(b), a)[E] <* IO.sync(ref.set(null))).uninterruptibly
                    } yield b

                  case IO.Tags.Uninterruptible =>
                    val io = curIo.asInstanceOf[IO.Uninterruptible[E, Any]]

                    curIo = for {
                      _ <- enterUninterruptible
                      v <- io.io
                      _ <- exitUninterruptible
                    } yield v

                  case IO.Tags.Sleep =>
                    val io = curIo.asInstanceOf[IO.Sleep[E]]

                    curIo = IO.async0 { callback =>
                      rts
                        .schedule(callback(SuccessUnit[E].asInstanceOf[ExitResult[E, Any]]), io.duration)
                        .asInstanceOf[Async[E, Any]]
                    }

                  case IO.Tags.Supervise =>
                    val io = curIo.asInstanceOf[IO.Supervise[E, Any]]

                    curIo = enterSupervision *>
                      io.value.ensuring(exitSupervision(io.error))

                  case IO.Tags.Terminate =>
                    val io = curIo.asInstanceOf[IO.Terminate[E, Any]]

                    val cause = io.cause

                    val finalizer = interruptStack[Void](cause)

                    if (finalizer eq null) {
                      // No finalizers, simply produce error:
                      eval = false
                      result = ExitResult.Terminated(cause)

                      // Report the termination cause to the supervisor:
                      rts.submit(rts.unsafePerformIO(unhandled(cause)))
                    } else {
                      // Must run finalizers first before failing:
                      val finalization = dispatchErrors(finalizer)
                      val completer    = io

                      // Do not interrupt finalization:
                      this.noInterrupt += 1

                      curIo = ensuringUninterruptibleExit(finalization[E] *> completer)
                    }

                  case IO.Tags.Supervisor =>
                    val value = unhandled

                    curIo = nextInstr[E](value, stack)

                    if (curIo eq null) {
                      eval = false
                      result = ExitResult.Completed(value)
                    }

                  case IO.Tags.Run =>
                    val io = curIo.asInstanceOf[IO.Run[E, _, Any]]

                    val value: FiberContext[E, Any] = fork(io.value, unhandled)

                    curIo = IO.async0[E, Any] { k =>
                      value.register { (v: ExitResult[E, Any]) =>
                        k(ExitResult.Completed(v))
                      } match {
                        case Async.Now(v)          => Async.Now(ExitResult.Completed(v))
                        case Async.MaybeLater(c)   => Async.MaybeLater(c)
                        case Async.MaybeLaterIO(c) => Async.MaybeLaterIO(c)
                      }
                    }
                }
              }
            } else {
              // Interruption cannot be interrupted:
              this.noInterrupt += 1

              curIo = IO.terminate[E, Any](die.get)
            }

            opcount = opcount + 1
          } while (eval)

          if (result ne null) {
            done(result.asInstanceOf[ExitResult[E, A]])
          }

          curIo = null // Ensure termination of outer loop
        } catch {
          // Catastrophic error handler. Any error thrown inside the interpreter is
          // either a bug in the interpreter or a bug in the user's code. Let the
          // fiber die but attempt finalization & report errors.
          case t: Throwable if (nonFatal(t)) =>
            // Interruption cannot be interrupted:
            this.noInterrupt += 1

            curIo = IO.terminate[E, Any](t)
        }
      }
    }

    final def fork[E, A](io: IO[E, A], handler: Throwable => IO[Void, Unit]): FiberContext[E, A] = {
      val context = new FiberContext[E, A](rts, handler)

      rts.submit(context.evaluate(io))

      context
    }

    /**
     * Resumes a synchronous evaluation given the newly produced value.
     *
     * @param value The value which will be used to resume the sync evaluation.
     */
    private final def resumeEvaluate(value: ExitResult[E, Any]): Unit =
      value match {
        case ExitResult.Completed(v) =>
          // Async produced a value:
          val io = nextInstr[E](v, stack)

          if (io eq null) done(value.asInstanceOf[ExitResult[E, A]])
          else evaluate(io)

        case ExitResult.Failed(t) => evaluate(IO.fail[E, Any](t))

        case ExitResult.Terminated(t) => evaluate(IO.terminate[E, Any](t))
      }

    /**
     * Resumes an asynchronous computation.
     *
     * @param value The value produced by the asynchronous computation.
     */
    private final def resumeAsync[A](value: ExitResult[E, Any]): Unit =
      if (shouldResumeAsync()) {
        // TODO: CPS transform
        // Take care not to overflow the stack in cases of 'deeply' nested
        // asynchronous callbacks.
        if (this.reentrancy > MaxResumptionDepth) {
          rts.submit(resumeEvaluate(value))
        } else resumeEvaluate(value)
      }

    private final def raceCallback[A, B](resume: ExitResult[E, IO[E, B]] => Unit,
                                         state: AtomicReference[RaceState],
                                         finish: A => IO[E, B]): ExitResult[E, A] => Unit =
      (tryA: ExitResult[E, A]) => {
        import RaceState._

        var loop = true
        var won  = false

        while (loop) {
          val oldStatus = state.get

          val newState = oldStatus match {
            case Finished =>
              won = false
              oldStatus
            case Started =>
              won = true
              Finished
          }

          loop = !state.compareAndSet(oldStatus, newState)
        }

        if (won) resume(tryA.map(finish))
      }

    private final def raceWith[A, B, C](unhandled: Throwable => IO[Void, Unit],
                                        leftIO: IO[E, A],
                                        rightIO: IO[E, B],
                                        finishLeft: (A, Fiber[E, B]) => IO[E, C],
                                        finishRight: (B, Fiber[E, A]) => IO[E, C]): IO[E, C] = {
      val left  = fork(leftIO, unhandled)
      val right = fork(rightIO, unhandled)

      // TODO: Interrupt raced fibers if parent is interrupted

      val leftWins  = (w: A) => finishLeft(w, right)
      val rightWins = (w: B) => finishRight(w, left)

      val state = new AtomicReference[RaceState](RaceState.Started)

      IO.flatten(IO.async0[E, IO[E, C]] { k =>
        var c1: Throwable => Unit = null
        var c2: Throwable => Unit = null

        left.register(raceCallback[A, C](k, state, leftWins)) match {
          case Async.Now(tryA) =>
            raceCallback[A, C](k, state, leftWins)(tryA)
          case Async.MaybeLater(cancel) =>
            c1 = cancel
          case Async.MaybeLaterIO(pureCancel) =>
            c1 = rts.impureCanceler(pureCancel)
        }

        right.register(raceCallback[B, C](k, state, rightWins)) match {
          case Async.Now(tryA) =>
            raceCallback[B, C](k, state, rightWins)(tryA)
          case Async.MaybeLater(cancel) =>
            c2 = cancel
          case Async.MaybeLaterIO(pureCancel) =>
            c2 = rts.impureCanceler(pureCancel)
        }

        val canceler = combineCancelers(c1, c2)

        if (canceler eq null) Async.later[E, IO[E, C]]
        else Async.maybeLater(canceler)
      })
    }

    final def changeErrorUnit[E2](cb: Callback[E2, Unit]): Callback[E, Unit] =
      changeError[E2, E, Unit](_ => SuccessUnit[E2], cb)

    final def interrupt[E2](t: Throwable): IO[E2, Unit] =
      IO.async0[E2, Unit](cb => kill0[E2](t, changeErrorUnit[E2](cb)))

    final def join: IO[E, A] = IO.async0(join0)

    final def enterSupervision: IO[E, Unit] = IO.sync {
      supervising += 1

      def newWeakSet[A]: Set[A] = Collections.newSetFromMap[A](new WeakHashMap[A, java.lang.Boolean]())

      val set = newWeakSet[FiberContext[_, _]]

      supervised = set :: supervised
    }

    final def supervise(child: FiberContext[_, _]): Unit =
      if (supervising > 0) {
        supervised match {
          case Nil =>
          case set :: _ =>
            set.add(child)

            ()
        }
      }

    @tailrec
    final def enterAsyncStart(): Int = {
      val oldStatus = status.get

      oldStatus match {
        case AsyncRegion(reentrancy, resume, cancel, joiners, killers) =>
          val newReentrancy = reentrancy + 1

          if (!status.compareAndSet(oldStatus, AsyncRegion(newReentrancy, resume + 1, cancel, joiners, killers)))
            enterAsyncStart()
          else newReentrancy

        case Executing(joiners, killers) =>
          val newReentrancy = 1

          if (!status.compareAndSet(oldStatus, AsyncRegion(newReentrancy, 1, None, joiners, killers))) enterAsyncStart()
          else newReentrancy

        case _ =>
          // If this is hit, there's a bug somewhere.
          throw new Error("Defect: Fiber is not in executing or async state")
      }
    }

    final def reentrancy: Int = status.get match {
      case AsyncRegion(v, _, _, _, _) => v

      case _ => 0
    }

    @tailrec
    final def enterAsyncEnd(): Unit = {
      val oldStatus = status.get

      oldStatus match {
        case AsyncRegion(1, 0, _, joiners, killers) =>
          // No more resumptions left and exiting last async boundary initiation:
          if (!status.compareAndSet(oldStatus, Executing(joiners, killers))) enterAsyncEnd()

        case AsyncRegion(reentrancy, resume, cancel, joiners, killers) =>
          if (!status.compareAndSet(oldStatus, AsyncRegion(reentrancy - 1, resume, cancel, joiners, killers)))
            enterAsyncEnd()

        case _ =>
      }
    }

    @tailrec
    final def awaitAsync(id: Int, c: Throwable => Unit): Unit = {
      val oldStatus = status.get

      oldStatus match {
        case AsyncRegion(reentrancy, resume, _, joiners, killers) if (id == reentrancy) =>
          if (!status.compareAndSet(oldStatus, AsyncRegion(reentrancy, resume, Some(c), joiners, killers)))
            awaitAsync(id, c)

        case _ =>
      }
    }

    @tailrec
    final def shouldResumeAsync(): Boolean = {
      val oldStatus = status.get

      oldStatus match {
        case AsyncRegion(0, 1, _, joiners, killers) =>
          // No more resumptions are left!
          if (!status.compareAndSet(oldStatus, Executing(joiners, killers))) shouldResumeAsync()
          else true

        case AsyncRegion(reentrancy, resume, _, joiners, killers) =>
          if (!status.compareAndSet(oldStatus, AsyncRegion(reentrancy, resume - 1, None, joiners, killers)))
            shouldResumeAsync()
          else true

        case _ => false
      }
    }

    final def exitSupervision[E2](e: Throwable): IO[E2, Unit] =
      IO.flatten(IO.sync {
        supervising -= 1

        var action = IO.unit[E2]

        supervised = supervised match {
          case Nil => Nil
          case set :: tail =>
            val iterator = set.iterator()

            while (iterator.hasNext()) {
              val child = iterator.next()

              action = action *> child.interrupt[E2](e)
            }

            tail
        }

        action
      })

    @inline
    final def shouldDie: Option[Throwable] =
      if (!killed || noInterrupt > 0) None
      else {
        val oldStatus = status.get

        oldStatus match {
          case Interrupting(error, _, _) => Some(error)
          case _                         => None
        }
      }

    final def ensuringUninterruptibleExit[Z](io: IO[E, Z]): IO[E, Z] =
      for {
        a <- io
        _ <- exitUninterruptible
      } yield a

    final def enterUninterruptible: IO[E, Unit] = IO.sync { noInterrupt += 1 }

    final def exitUninterruptible: IO[E, Unit] = IO.sync { noInterrupt -= 1 }

    final def register(cb: Callback[E, A]): Async[E, A] = join0(cb)

    @tailrec
    final def done(v: ExitResult[E, A]): Unit = {
      val oldStatus = status.get

      oldStatus match {
        case Executing(joiners, killers) =>
          if (!status.compareAndSet(oldStatus, Done(v))) done(v)
          else purgeJoinersKillers(v, joiners, killers)

        case Interrupting(_, joiners, killers) =>
          if (!status.compareAndSet(oldStatus, Done(v))) done(v)
          else purgeJoinersKillers(v, joiners, killers)

        case AsyncRegion(_, _, _, joiners, killers) =>
          // TODO: Guard against errant `done` or not?
          if (!status.compareAndSet(oldStatus, Done(v))) done(v)
          else purgeJoinersKillers(v, joiners, killers)

        case Done(_) => // Huh?
      }
    }

    def changeError[E1, E2, A](f: E2 => ExitResult[E1, A], cb: Callback[E1, A]): Callback[E2, A] = {
      case ExitResult.Completed(a)  => cb(ExitResult.Completed(a))
      case ExitResult.Terminated(t) => cb(ExitResult.Terminated(t))
      case ExitResult.Failed(e2)    => cb(f(e2))
    }
    @tailrec
    private final def kill0[E2](t: Throwable, cb: Callback[E, Unit]): Async[E2, Unit] = {
      killed = true

      val oldStatus = status.get

      oldStatus match {
        case Executing(joiners, killers) =>
          if (!status.compareAndSet(oldStatus, Interrupting(t, joiners, cb :: killers))) kill0(t, cb)
          else Async.later[E2, Unit]

        case Interrupting(t, joiners, killers) =>
          if (!status.compareAndSet(oldStatus, Interrupting(t, joiners, cb :: killers))) kill0(t, cb)
          else Async.later[E2, Unit]

        case AsyncRegion(_, resume, cancelOpt, joiners, killers) if (resume > 0 && noInterrupt == 0) =>
          val v = ExitResult.Terminated[E, A](t)

          if (!status.compareAndSet(oldStatus, Done(v))) kill0(t, cb)
          else {
            // We interrupted async before it could resume. Now we have to
            // cancel the computation, if possible, and handle any finalizers.
            cancelOpt match {
              case None =>
              case Some(cancel) =>
                try cancel(t)
                catch {
                  case t: Throwable if (nonFatal(t)) =>
                    fork(unhandled(t), unhandled)
                }
            }

            val finalizer = interruptStack[Void](t)

            if (finalizer ne null) {
              fork[Void, Unit](dispatchErrors(finalizer), unhandled)
            }

            purgeJoinersKillers(v, joiners, killers)

            Async.now(SuccessUnit[E2])
          }

        case AsyncRegion(_, _, _, joiners, killers) =>
          if (!status.compareAndSet(oldStatus, Interrupting(t, joiners, cb :: killers))) kill0(t, cb)
          else Async.later[E2, Unit]

        case Done(_) => Async.now(SuccessUnit[E2])
      }
    }

    @tailrec
    private final def join0(cb: Callback[E, A]): Async[E, A] = {
      val oldStatus = status.get

      oldStatus match {
        case Executing(joiners, killers) =>
          if (!status.compareAndSet(oldStatus, Executing(cb :: joiners, killers))) join0(cb)
          else Async.later[E, A]

        case Interrupting(t, joiners, killers) =>
          if (!status.compareAndSet(oldStatus, Interrupting(t, cb :: joiners, killers))) join0(cb)
          else Async.later[E, A]

        case AsyncRegion(reenter, resume, cancel, joiners, killers) =>
          if (!status.compareAndSet(oldStatus, AsyncRegion(reenter, resume, cancel, cb :: joiners, killers))) join0(cb)
          else Async.later[E, A]

        case Done(v) => Async.now(v)
      }
    }

    private final def purgeJoinersKillers(v: ExitResult[E, A],
                                          joiners: List[Callback[E, A]],
                                          killers: List[Callback[E, Unit]]): Unit = {
      // To preserve fair scheduling, we submit all resumptions on the thread
      // pool in (rough) order of their submission.
      killers.reverse.foreach(k => rts.submit(k(SuccessUnit[E])))
      joiners.foreach(k => rts.submit(k(v)))
    }
  }

  sealed trait FiberStatus[E, A]
  object FiberStatus {
    final case class Executing[E, A](joiners: List[Callback[E, A]], killers: List[Callback[E, Unit]])
        extends FiberStatus[E, A]
    final case class Interrupting[E, A](error: Throwable,
                                        joiners: List[Callback[E, A]],
                                        killers: List[Callback[E, Unit]])
        extends FiberStatus[E, A]
    final case class AsyncRegion[E, A](reentrancy: Int,
                                       resume: Int,
                                       cancel: Option[Throwable => Unit],
                                       joiners: List[Callback[E, A]],
                                       killers: List[Callback[E, Unit]])
        extends FiberStatus[E, A]
    final case class Done[E, A](value: ExitResult[E, A]) extends FiberStatus[E, A]

    def Initial[E, A] = Executing[E, A](Nil, Nil)
  }

  val _SuccessUnit: ExitResult[Nothing, Unit] = ExitResult.Completed(())

  final def SuccessUnit[E]: ExitResult[E, Unit] = _SuccessUnit.asInstanceOf[ExitResult[E, Unit]]

  final def combineCancelers(c1: Throwable => Unit, c2: Throwable => Unit): Throwable => Unit =
    if (c1 eq null) {
      if (c2 eq null) null
      else c2
    } else if (c2 eq null) {
      c1
    } else
      (t: Throwable) => {
        c1(t)
        c2(t)
      }
}
