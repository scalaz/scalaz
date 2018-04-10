// Copyright (C) 2017-2018 John A. De Goes. All rights reserved.
package scalaz.effect

import scala.annotation.switch
import scala.annotation.tailrec
import scala.concurrent.duration.Duration

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.{Executors, TimeUnit}
import java.lang.{Runnable, Runtime}

import scalaz.data.Disjunction._
import scalaz.data.Maybe

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
    case FiberResult.Completed(v) => v
    case FiberResult.Interrupted(t) => throw t
    case FiberResult.Failed(e) => throw Errors.UnhandledError(e)
  }

  /**
   * Effectfully interprets an `IO`, blocking if necessary to obtain the result.
   */
  final def tryUnsafePerformIO[E, A](io: IO[E, A]): FiberResult[E, A] = {
    // TODO: Optimize — this is slow and inefficient
    var result: FiberResult[E, A] = null
    lazy val lock = new ReentrantLock()
    lazy val done = lock.newCondition()

    val context = new FiberContext[E, A](this, defaultHandler)

    context.evaluate(io)

    (context.register { (r: FiberResult[E, A]) =>
      lock.lock()

      try {
        result = r

        done.signal()
      } finally lock.unlock()
    }) match {
      case AsyncReturn.Now(v) =>
        result = v

      case _ =>
        while (result == null) {
          lock.lock()
          try done.await()
          finally lock.unlock()
        }
    }

    result
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
  val threadPool = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors().max(2))

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

  final def schedule[E, A](block: => A, duration: Duration): AsyncReturn[E, Unit] =
    if (duration == Duration.Zero) {
      submit(block)

      AsyncReturn.later[E, Unit]
    } else {
      val future = scheduledExecutor.schedule(
        new Runnable {
          def run: Unit = { submit(block) }
        }, duration.toNanos, TimeUnit.NANOSECONDS)

      AsyncReturn.maybeLater { (t: Throwable) => future.cancel(true); () }
    }
}

private object RTS {
  // Utility function to avoid catching truly fatal exceptions. Do not allocate
  // memory here since this would defeat the point of checking for OOME.
  def nonFatal(t: Throwable): Boolean =
    !t.isInstanceOf[InternalError] && !t.isInstanceOf[OutOfMemoryError]

  sealed trait RaceState
  object RaceState {
    case object Started     extends RaceState
    case object OtherFailed extends RaceState
    case object Finished    extends RaceState
  }

  type Callback[E, A] = FiberResult[E, A] => Unit

  @inline
  final def nextInstr[E](value: Any, stack: Stack): IO[E, Any] =
    // TODO: Eliminate type cast
    if (!stack.isEmpty()) stack.pop()(value).asInstanceOf[IO[E, Any]] else null

  object Catcher extends Function[Any, IO[Any, Any]] {
    final def apply(v: Any): IO[Any, Any] = IO.now(\/-(v))
  }

  object IdentityCont extends Function[Any, IO[Any, Any]] {
    final def apply(v: Any): IO[Any, Any] = IO.now(v)
  }

  final case class Finalizer[E](finalizer: FiberResult[E, Any] => IO[E, Unit]) extends Function[Any, IO[E, Any]] {
    final def apply(v: Any): IO[E, Any] = IO.now(v)
  }

  final class Stack() {
    type Cont = Any => IO[_, Any]

    private[this] var array = new Array[Cont](10)
    private[this] var size = 0

    final def peek(): Cont = array(size - 1)

    final def isEmpty(): Boolean = size == 0

    final def push(a: Cont): Unit = {
      if (size == array.length) {
        val array2 = new Array[Cont](array.length + (array.length >> 1))

        Array.copy(array, 0, array2, 0, array.length)

        array = array2
      }
      array(size) = a
      size = size + 1
    }

    final def pop(): Cont = {
      val idx = size - 1

      val a = array(idx)

      array(idx) = null.asInstanceOf[Cont] // GC

      size = idx

      a
    }
  }

  /**
   * An implementation of Fiber that maintains context necessary for evaluation.
   */
  final class FiberContext[E, A](rts: RTS, val unhandled: Throwable => IO[E, Unit]) extends Fiber[E, A] {
    import FiberStatus._
    import java.util.{WeakHashMap, Collections, Set}
    import rts.{YieldMaxOpCount, MaxResumptionDepth}

    // Accessed from multiple threads:
    private[this] val status = new AtomicReference[FiberStatus[E, A]](FiberStatus.Initial[E, A])
    //@volatile
    private[this] var killed = false

    // TODO: A lot can be pulled out of status to increase performance

    // Accessed from within a single thread (not necessarily the same):
    private[this] var noInterrupt = 0
    private[this] var supervised : List[Set[FiberContext[_, _]]] = Nil
    private[this] var supervising = 0

    private[this] val stack: Stack = new Stack()

    /**
     * Creates an action to dispatch a list of errors to the fiber's uncaught
     * error handler.
     *
     * @param errors  The effectfully produced list of errors, in reverse order.
     */
    final def dispatchErrors(errors: IO[E, List[E]]): IO[E, Unit] =
      errors.flatMap(errors =>
        // Each error produced by a finalizer must be handled using the
        // context's unhandled exception handler:
        errors.reverse.map { error =>
          unhandled(Errors.UnhandledError(error))
        }.foldLeft(IO.unit[E])(_ *> _)
      )

    /**
     * Catches an exception, returning a (possibly null) finalizer action that
     * must be executed. It is painstakingly *guaranteed* that the stack will be
     * empty in the sole case the exception was not caught by any exception
     * handler—i.e. the exceptional case.
     *
     * @param err   The exception that is being thrown.
     */
    final def catchException[E2](err: E): IO[E2, List[E]] = {
      var finalizer: IO[E2, List[E]] = null
      var body: FiberResult[E, Any] = null

      var caught = false

      // Unwind the stack, looking for exception handlers and coalescing
      // finalizers.
      while (!caught && !stack.isEmpty()) {
        stack.pop() match {
          case `Catcher` => caught = true
          case f0 : Finalizer[_] =>
            val f = f0.asInstanceOf[Finalizer[E]]

            // Lazy initialization of body:
            if (body == null) body = FiberResult.Failed(err)

            val currentFinalizer: IO[E2, List[E]] = f.finalizer(body).attempt[E2].map {
              case -\/(error) => error :: Nil
              case _ => Nil
            }

            if (finalizer == null) finalizer = currentFinalizer
            else finalizer = for {
              oldErrors <- finalizer
              newErrors <- currentFinalizer
            } yield newErrors ::: oldErrors

          case _ =>
        }
      }

      // This may seem strange but we need to maintain the invariant that an
      // empty stack means the exception was *not* caught. So if the exception
      // was caught but the stack is empty, we make the stack non-empty. This
      // lets us return only the finalizer, which will be null for common cases,
      // and result in zero heap allocations for the happy path.
      if (caught && stack.isEmpty()) stack.push(IdentityCont)

      finalizer
    }

    /**
     * Empties the stack, collecting all finalizers and coalescing them into an
     * action that produces a list (possibly empty) of errors during finalization.
     *
     * @param error The error being used to interrupt the fiber.
     */
    final def interruptStack[E2](error: Throwable): IO[E2, List[E]] = {
      // Use null to achieve zero allocs for the common case of no finalizers:
      var finalizer: IO[E2, List[E]] = null

      if (!stack.isEmpty()) {
        // Any finalizers will require FiberResult. Here we fake lazy evaluation
        // to eliminate unnecessary allocation:
        var body: FiberResult[E, Any] = null

        while (!stack.isEmpty()) {
          // Peel off all the finalizers, composing them into a single finalizer
          // that produces a possibly empty list of errors that occurred when
          // executing the finalizers. The order of errors is outer-to-inner
          // (reverse chronological).
          stack.pop() match {
            case f0 : Finalizer[_] =>
              val f = f0.asInstanceOf[Finalizer[E]]

              // Lazy initialization of body:
              if (body == null) body = FiberResult.Interrupted(error)

              val currentFinalizer = f.finalizer(body).attempt[E2].map {
                case -\/(t) => t :: Nil
                case _ => Nil
              }

              if (finalizer == null) finalizer = currentFinalizer
              else finalizer = for {
                oldErrors <- finalizer
                newErrors <- currentFinalizer
              } yield newErrors ::: oldErrors

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
      var curIo : IO[E, Any] = io0.asInstanceOf[IO[E, Any]]

      while (curIo != null) {
        try {
          // Put the maximum operation count on the stack for fast access:
          val maxopcount = YieldMaxOpCount

          var result  : FiberResult[E, Any] = null
          var eval    : Boolean               = true
          var opcount : Int                   = 0

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
                (curIo.tag : @switch) match {
                  case IO.Tags.FlatMap =>
                    val io = curIo.asInstanceOf[IO.FlatMap[E, Any, Any]]

                    val nested = io.io

                    // A mini interpreter for the left side of FlatMap that evaluates
                    // anything that is 1-hop away. This eliminates heap usage for the
                    // happy path.
                    (nested.tag : @switch) match {
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

                    if (curIo == null) {
                      eval   = false
                      result = FiberResult.Completed(value)
                    }

                  case IO.Tags.Strict =>
                    val io = curIo.asInstanceOf[IO.Strict[E, Any]]

                    val value = io.value

                    curIo = nextInstr[E](value, stack)

                    if (curIo == null) {
                      eval   = false
                      result = FiberResult.Completed(value)
                    }

                  case IO.Tags.SyncEffect =>
                    val io = curIo.asInstanceOf[IO.SyncEffect[E, Any]]

                    val value = io.effect()

                    curIo = nextInstr[E](value, stack)

                    if (curIo == null) {
                      eval   = false
                      result = FiberResult.Completed(value)
                    }

                  case IO.Tags.Fail =>
                    val io = curIo.asInstanceOf[IO.Fail[E, Any]]

                    val error = io.failure

                    val finalizer = catchException[E](error)

                    if (stack.isEmpty()) {
                      // Exception not caught, stack is empty:
                      if (finalizer == null) {
                        // No finalizer, so immediately produce the error.
                        eval   = false
                        result = FiberResult.Failed(error)
                      } else {
                        // We have finalizers to run. We'll resume executing with the
                        // uncaught failure after we have executed all the finalizers:
                        val finalization  = dispatchErrors(finalizer)
                        val completer     = IO.fail[E, Any](error)

                        // Do not interrupt finalization:
                        this.noInterrupt += 1

                        curIo = ensuringUninterruptibleExit(finalization *> completer)
                      }
                    } else {
                      // Exception caught:
                      if (finalizer == null) {
                        // No finalizer to run:
                        curIo = nextInstr[E](-\/(error), stack)

                        if (curIo == null) {
                          eval   = false
                          result = FiberResult.Failed(error)
                        }
                      } else {
                        // Must run finalizer first:
                        val finalization = dispatchErrors(finalizer)
                        val completer    = IO.fail[E, Any](error)

                        // Do not interrupt finalization:
                        this.noInterrupt += 1

                        curIo = ensuringUninterruptibleExit(finalization *> completer)
                      }
                    }

                  case IO.Tags.AsyncEffect =>
                    val io = curIo.asInstanceOf[IO.AsyncEffect[E, Any]]

                    val id = enterAsyncStart()

                    try {
                      io.register(resumeAsync) match {
                        case AsyncReturn.Now(value) =>
                          // Value returned synchronously, callback will never be
                          // invoked. Attempt resumption now:
                          if (shouldResumeAsync()) {
                            value match {
                              case FiberResult.Completed(v) =>
                                curIo = nextInstr[E](v, stack)

                                if (curIo == null) {
                                  eval   = false
                                  result = value
                                }
                              case FiberResult.Interrupted(t) =>
                                curIo = IO.Interrupt(t)
                              case FiberResult.Failed(e) =>
                                curIo = IO.Fail(e)
                            }
                          } else {
                            // Completion handled by interruptor:
                            eval = false
                          }

                        case AsyncReturn.MaybeLater(canceler) =>
                          // We have a canceler, attempt to store a reference to
                          // it in case the async computation is interrupted:
                          awaitAsync(id, canceler)

                          eval = false

                        case _ =>
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
                            case FiberResult.Completed(v) =>
                              curIo = nextInstr[E](v, stack)

                              if (curIo == null) {
                                eval   = false
                                result = value.asInstanceOf[FiberResult[E,Any]]
                              }
                            case FiberResult.Interrupted(t) =>
                              curIo = IO.Interrupt(t)
                            case FiberResult.Failed(e) =>
                              curIo = IO.Fail(e)
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
                    val io = curIo.asInstanceOf[IO.Fork[E, Any]]

                    val optHandler = Maybe.toOption(io.handler)

                    val handler = if (optHandler eq None) unhandled else optHandler.get

                    val value: FiberContext[E, Any] = fork(io.value, handler)

                    supervise(value)

                    curIo = nextInstr[E](value : Fiber[E, Any], stack)

                    if (curIo == null) {
                      eval   = false
                      result = FiberResult.Completed(value)
                    }

                  case IO.Tags.Race =>
                    val io = curIo.asInstanceOf[IO.Race[E, Any, Any, Any]]

                    curIo = raceWith(unhandled, io.left, io.right, io.finish)

                  case IO.Tags.Suspend =>
                    val io = curIo.asInstanceOf[IO.Suspend[E, Any]]

                    curIo = io.value()

                  case IO.Tags.Bracket =>
                    val io = curIo.asInstanceOf[IO.Bracket[E, Any, Any]]

                    val ref = new AtomicReference[Any]()

                    val finalizer = Finalizer[E](rez =>
                      IO.suspend(if (ref.get != null) io.release(rez, ref.get) else IO.unit))

                    stack.push(finalizer)

                    // TODO: Optimize
                    // FIXME: Handle case of release throwing error.
                    curIo =
                      for {
                        a <- (for {
                               a <- io.acquire
                               _ <- IO.sync(ref.set(a))
                             } yield a).uninterruptibly
                        b <- io.use(a)
                        _ <- (io.release(FiberResult.Completed(b), a) <* IO.sync(ref.set(null))).uninterruptibly
                      } yield b

                  case IO.Tags.Uninterruptible =>
                    val io = curIo.asInstanceOf[IO.Uninterruptible[E, Any]]

                    curIo = IO.absolve(for {
                      _ <- enterUninterruptible
                      v <- io.io.attempt
                      _ <- exitUninterruptible
                    } yield v)

                  case IO.Tags.Sleep =>
                    val io = curIo.asInstanceOf[IO.Sleep[E]]

                    curIo = IO.AsyncEffect { callback =>
                      rts.schedule(callback(SuccessUnit[E].asInstanceOf[FiberResult[E, Any]]), io.duration).asInstanceOf[AsyncReturn[E, Any]]
                    }

                  case IO.Tags.Supervise =>
                    val io = curIo.asInstanceOf[IO.Supervise[E, Any]]

                    curIo = enterSupervision *>
                      io.value.ensuring(exitSupervision(io.error))

                  case IO.Tags.Interrupt =>
                    val io = curIo.asInstanceOf[IO.Interrupt[E, Any]]

                    val finalizer = interruptStack[E](io.failure)

                    if (finalizer == null) {
                      // No finalizers, simply produce error:
                      eval    = false
                      result  = FiberResult.Interrupted(io.failure)
                    } else {
                      // Must run finalizers first before failing:
                      val finalization = dispatchErrors(finalizer)
                      val completer    = IO.Interrupt[E, Any](io.failure)

                      // Do not interrupt finalization:
                      this.noInterrupt += 1

                      // FIXME: Call uncaught error handler

                      curIo = ensuringUninterruptibleExit(finalization *> completer)
                    }
                }
              }
            } else {
              // Interruption cannot be interrupted:
              this.noInterrupt += 1

              curIo = IO.Interrupt[E, Any](die.get)
            }

            opcount = opcount + 1
          } while (eval)

          if (result != null) {
            done(result.asInstanceOf[FiberResult[E, A]])
          }

          curIo = null // Ensure termination of outer loop
        } catch {
          // Catastrophic error handler. Any error thrown inside the interpreter is
          // either a bug in the interpreter or a bug in the user's code. Let the
          // fiber die but attempt finalization & report errors.
          case t : Throwable if (nonFatal(t)) =>
            // Interruption cannot be interrupted:
            this.noInterrupt += 1

            curIo = IO.Interrupt[E, Any](t)
        }
      }
    }

    final def fork[E, A](io: IO[E, A], handler: Throwable => IO[E, Unit]): FiberContext[E, A] = {
      val context = new FiberContext[E, A](rts, handler)

      rts.submit(context.evaluate(io))

      context
    }

    /**
     * Resumes a synchronous evaluation given the newly produced value.
     *
     * @param value The value which will be used to resume the sync evaluation.
     */
    private final def resumeEvaluate(value: FiberResult[E, Any]): Unit = {
      value match {
        case FiberResult.Completed(v) =>
          // Async produced a value:
          val io = nextInstr[E](v, stack)

          if (io == null) done(value.asInstanceOf[FiberResult[E, A]])
          else evaluate(io)

        case FiberResult.Failed(t) => evaluate(IO.Fail[E, Any](t))

        case FiberResult.Interrupted(t) => evaluate(IO.Interrupt[E, Any](t))
      }
    }

    /**
     * Resumes an asynchronous computation.
     *
     * @param value The value produced by the asynchronous computation.
     */
    private final def resumeAsync[A](value: FiberResult[E, Any]): Unit = {
      if (shouldResumeAsync()) {
        // TODO: CPS transform
        // Take care not to overflow the stack in cases of 'deeply' nested
        // asynchronous callbacks.
        if (this.reentrancy > MaxResumptionDepth) {
          rts.submit(resumeEvaluate(value))
        } else resumeEvaluate(value)
      }
    }

    private final def raceWith[A, B, C](unhandled: Throwable => IO[E, Unit], leftIO: IO[E, A], rightIO: IO[E, B], finish: (A, Fiber[E, B]) \/ (B, Fiber[E, A]) => IO[E, C]): IO[E, C] = {
      import RaceState._

      val left = fork(leftIO, unhandled)
      val right = fork(rightIO, unhandled)

      // TODO: Interrupt raced fibers if parent is interrupted?

      val leftWins = (w: A, r: Fiber[E, B]) => finish(-\/((w, r)))
      val rightWins = (w: B, l: Fiber[E, A]) => finish(\/-((w, l)))

      IO.flatten(IO.async0[E, IO[E, C]] { resume =>
        val state = new AtomicReference[RaceState](Started)

        def callback[A1, B1](other: Fiber[E, B1], finish: (A1, Fiber[E, B1]) => IO[E, C]): FiberResult[E, A1] => Unit = (tryA: FiberResult[E, A1]) => {
          var loop = true
          var action: () => Unit = null

          while (loop) {
            val oldStatus = state.get

            val newState = oldStatus match {
              case Finished => oldStatus
              case OtherFailed =>
                tryA match {
                  case FiberResult.Completed(a) =>
                    action = () => {
                      resume(FiberResult.Completed(finish(a, other)))
                    }
                    Finished

                  case FiberResult.Failed(e) =>
                    action = () => {
                      resume(FiberResult.Failed(e))
                    }
                    Finished

                  case FiberResult.Interrupted(e) =>
                    action = () => {
                      resume(FiberResult.Interrupted(e))
                    }
                    Finished
                }
              case Started =>
                tryA match {
                  case FiberResult.Completed(a) =>
                    action = () => {
                      resume(FiberResult.Completed(finish(a, other)))
                    }
                    Finished

                  case _ => OtherFailed
                }
            }

            if (state.compareAndSet(oldStatus, newState)) loop = false
          }

          if (action != null) action()
        }

        var canceler: Throwable => Unit = null

        // TODO: Tighten this up a bit by dealing with synchronous returns.
        left.register(callback(right, leftWins)) match {
          case AsyncReturn.Now(tryA) => callback(right, leftWins)(tryA)
          case AsyncReturn.MaybeLater(cancel) =>
            canceler = cancel
          case _ =>
        }

        right.register(callback(left, rightWins)) match {
          case AsyncReturn.Now(tryA) => callback(left, rightWins)(tryA)
          case AsyncReturn.MaybeLater(cancel) =>
            if (canceler == null) canceler = cancel
            else {
              val oldCanceler = canceler

              canceler = (t: Throwable) => {
                try oldCanceler(t)
                catch {
                  case t : Throwable if (nonFatal(t)) => // FIXME: Don't ignore
                }

                cancel(t)
              }
            }
          case _ =>
        }

        if (canceler == null) AsyncReturn.later[E, IO[E, C]]
        else AsyncReturn.maybeLater(canceler)
      })
    }

    final def interrupt(t: Throwable): IO[E, Unit] = IO.async0(kill0(t, _))

    final def join: IO[E, A] = IO.async0(join0)

    final def enterSupervision: IO[E, Unit] = IO.sync {
      supervising += 1

      def newWeakSet[A]: Set[A] = Collections.newSetFromMap[A](new WeakHashMap[A, java.lang.Boolean]())

      val set = newWeakSet[FiberContext[_, _]]

      supervised = set :: supervised
    }

    final def supervise(child: FiberContext[_, _]): Unit = {
      if (supervising > 0) {
        supervised match {
          case Nil =>
          case set :: _ =>
            set.add(child)

            ()
        }
      }
    }

    @tailrec
    final def enterAsyncStart(): Int = {
      val oldStatus = status.get

      oldStatus match {
        case AsyncRegion(reentrancy, resume, cancel, joiners, killers) =>
          val newReentrancy = reentrancy + 1

          if (!status.compareAndSet(oldStatus, AsyncRegion(newReentrancy, resume + 1, cancel, joiners, killers))) enterAsyncStart()
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
          if (!status.compareAndSet(oldStatus, AsyncRegion(reentrancy - 1, resume, cancel, joiners, killers))) enterAsyncEnd()

        case _ =>
      }
    }

    @tailrec
    final def awaitAsync(id: Int, c: Throwable => Unit): Unit = {
      val oldStatus = status.get

      oldStatus match {
        case AsyncRegion(reentrancy, resume, _, joiners, killers) if (id == reentrancy) =>
          if (!status.compareAndSet(oldStatus, AsyncRegion(reentrancy, resume, Some(c), joiners, killers))) awaitAsync(id, c)

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
          if (!status.compareAndSet(oldStatus, AsyncRegion(reentrancy, resume - 1, None, joiners, killers))) shouldResumeAsync()
          else true

        case _ => false
      }
    }

    final def exitSupervision(e: Throwable): IO[E, Unit] = IO.flatten(IO.sync {
      supervising -= 1

      var action = IO.unit[E]

      supervised = supervised match {
        case Nil => Nil
        case set :: tail =>
          val iterator = set.iterator()

          while (iterator.hasNext()) {
            val child = iterator.next()

            // TODO: Collect & dispatch errors, will also eliminate need for
            // type cast.
            action = action *> child.interrupt(e).asInstanceOf[IO[E, Unit]]
          }

          tail
      }

      action
    })

    @inline
    final def shouldDie: Option[Throwable] = if (!killed || noInterrupt > 0) None else {
      val oldStatus = status.get

      oldStatus match {
        case Interrupting(error, _, _) => Some(error)
        case _ => None
      }
    }

    final def ensuringUninterruptibleExit[Z](io: IO[E, Z]): IO[E, Z] =
      IO.absolve(io.attempt <* exitUninterruptible)

    final def enterUninterruptible: IO[E, Unit] = IO.sync { noInterrupt += 1 }

    final def exitUninterruptible: IO[E, Unit] = IO.sync { noInterrupt -= 1 }

    final def register(cb: Callback[E, A]): AsyncReturn[E, A] = join0(cb)

    @tailrec
    final def done(v: FiberResult[E, A]): Unit = {
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

    @tailrec
    private final def kill0(t: Throwable, cb: Callback[E, Unit]): AsyncReturn[E, Unit] = {
      killed = true

      val oldStatus = status.get

      oldStatus match {
        case Executing(joiners, killers) =>
          if (!status.compareAndSet(oldStatus, Interrupting(t, joiners, cb :: killers))) kill0(t, cb)
          else AsyncReturn.later[E, Unit]

        case Interrupting(t, joiners, killers) =>
          if (!status.compareAndSet(oldStatus, Interrupting(t, joiners, cb :: killers))) kill0(t, cb)
          else AsyncReturn.later[E, Unit]

        case AsyncRegion(_, resume, cancelOpt, joiners, killers) if (resume > 0 && noInterrupt == 0) =>
          val v = FiberResult.Interrupted[E, A](t)

          if (!status.compareAndSet(oldStatus, Done(v))) kill0(t, cb)
          else {
            // We interrupted async before it could resume. Now we have to
            // cancel the computation, if possible, and handle any finalizers.
            cancelOpt match {
              case None =>
              case Some(cancel) =>
                try cancel(t) catch { case t : Throwable if (nonFatal(t)) => /* TODO: Don't throw away? */ }
            }

            val finalizer = interruptStack[E](t)

            if (finalizer != null) {
              fork(dispatchErrors(finalizer), unhandled)
            }

            purgeJoinersKillers(v, joiners, killers)

            AsyncReturn.now(SuccessUnit[E])
          }

        case AsyncRegion(_, _, _, joiners, killers) =>
          if (!status.compareAndSet(oldStatus, Interrupting(t, joiners, cb :: killers))) kill0(t, cb)
          else AsyncReturn.later[E, Unit]

        case Done(_) => AsyncReturn.now(SuccessUnit[E])
      }
    }

    @tailrec
    private final def join0(cb: Callback[E, A]): AsyncReturn[E, A] = {
      val oldStatus = status.get

      oldStatus match {
        case Executing(joiners, killers) =>
          if (!status.compareAndSet(oldStatus, Executing(cb :: joiners, killers))) join0(cb)
          else AsyncReturn.later[E, A]

        case Interrupting(t, joiners, killers) =>
          if (!status.compareAndSet(oldStatus, Interrupting(t, cb :: joiners, killers))) join0(cb)
          else AsyncReturn.later[E, A]

        case AsyncRegion(reenter, resume, cancel, joiners, killers) =>
          if (!status.compareAndSet(oldStatus, AsyncRegion(reenter, resume, cancel, cb :: joiners, killers))) join0(cb)
          else AsyncReturn.later[E, A]

        case Done(v) => AsyncReturn.now(v)
      }
    }

    private final def purgeJoinersKillers(v: FiberResult[E, A], joiners: List[Callback[E, A]], killers: List[Callback[E, Unit]]): Unit = {
      // FIXME: Put all but one of these (first joiner?) on the thread pool.
      killers.reverse.foreach(_.apply(SuccessUnit[E]))
      joiners.reverse.foreach(_.apply(v))
    }
  }

  sealed trait FiberStatus[E, A]
  object FiberStatus {
    final case class Executing[E, A](joiners: List[Callback[E, A]], killers: List[Callback[E, Unit]]) extends FiberStatus[E, A]
    final case class Interrupting[E, A](error: Throwable, joiners: List[Callback[E, A]], killers: List[Callback[E, Unit]]) extends FiberStatus[E, A]
    final case class AsyncRegion[E, A](reentrancy: Int, resume: Int, cancel: Option[Throwable => Unit], joiners: List[Callback[E, A]], killers: List[Callback[E, Unit]]) extends FiberStatus[E, A]
    final case class Done[E, A](value: FiberResult[E, A]) extends FiberStatus[E, A]

    def Initial[E, A] = Executing[E, A](Nil, Nil)
  }

  val _SuccessUnit: FiberResult[Nothing, Unit] = FiberResult.Completed(())

  def SuccessUnit[E]: FiberResult[E, Unit] = _SuccessUnit.asInstanceOf[FiberResult[E, Unit]]
}
