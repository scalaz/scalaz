// Copyright (C) 2017 John A. De Goes. All rights reserved.
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
   * Effectfully and synchronously interprets an `IO[A]`, either throwing an
   * error, running forever, or producing an `A`.
   */
  def unsafePerformIO[A](io: IO[A]): A = extract(tryUnsafePerformIO(io))

  /**
   * Effectfully interprets an `IO`, blocking if necessary to obtain the result.
   */
  final def tryUnsafePerformIO[A](io: IO[A]): Throwable \/ A = {
    // TODO: Optimize — this is slow and inefficient
    var result: Try[A] = null
    lazy val lock = new ReentrantLock()
    lazy val done = lock.newCondition()

    val context = new FiberContext[A](this, defaultHandler)

    context.evalSync(io)

    (context.register { (r: Try[A]) =>
      try {
        lock.lock()

        result = r

        done.signal()
      } finally lock.unlock()
    }) match {
      case AsyncReturn.Now(v) =>
        result = v.asInstanceOf[Try[A]]

      case _ =>
        while (result == null) try {
          lock.lock()
          done.await()
        } finally lock.unlock()
    }

    result
  }

  /**
   * The default handler for unhandled exceptions in the main fiber, and any
   * fibers it forks that recursively inherit the handler.
   */
  val defaultHandler: Throwable => IO[Unit] =
    (t: Throwable) => IO.sync(t.printStackTrace())

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
  val YieldMaxOpCount = 1024

  lazy val scheduledExecutor = Executors.newScheduledThreadPool(1)

  final def submit[A](block: => A): Unit = {
    threadPool.submit(new Runnable {
      def run: Unit = { block; () }
    })

    ()
  }

  final def schedule[A](block: => A, duration: Duration): AsyncReturn[Unit] =
    if (duration == Duration.Zero) {
      submit(block)

      AsyncReturn.later[Unit]
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
    if (t.isInstanceOf[InternalError]) false
    else if (t.isInstanceOf[OutOfMemoryError]) false
    else true

  sealed trait RaceState
  object RaceState {
    case object Started     extends RaceState
    case object OtherFailed extends RaceState
    case object Finished    extends RaceState
  }

  type Try[A] = Throwable \/ A

  def Try[A](a: => A): Try[A] =
    try \/-(a) catch { case t : Throwable if (nonFatal(t)) => -\/(t) }

  type Callback[A] = Try[A] => Unit

  final def extract[A](t: Try[A]): A = t match {
    case -\/(t) => throw t
    case \/-(v) => v
  }

  @inline
  final def nextInstr(value: Any, stack: Stack): IO[Any] =
    if (!stack.isEmpty()) stack.pop()(value) else null

  object Catcher extends Function[Any, IO[Any]] {
    final def apply(v: Any): IO[Any] = IO.now(\/-(v))
  }

  object IdentityCont extends Function[Any, IO[Any]] {
    final def apply(v: Any): IO[Any] = IO.now(v)
  }

  final case class Finalizer(finalizer: BracketResult[Any] => IO[Unit]) extends Function[Any, IO[Any]] {
    final def apply(v: Any): IO[Any] = IO.now(v)
  }

  final class Stack() {
    type Cont = Any => IO[Any]

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
  final class FiberContext[A](rts: RTS, val unhandled: Throwable => IO[Unit]) extends Fiber[A] {
    import FiberStatus._
    import java.util.{WeakHashMap, Collections, Set}
    import rts.{YieldMaxOpCount, MaxResumptionDepth}

    // Accessed from multiple threads:
    private[this] val status = new AtomicReference[FiberStatus[A]](FiberStatus.Initial[A])
    @volatile private[this] var killed = false

    // TODO: A lot can be pulled out of status to increase performance

    // Accessed from within a single thread (not necessarily the same):
    private[this] var noInterrupt = 0
    private[this] var supervised : List[Set[FiberContext[_]]] = Nil
    private[this] var supervising = 0

    final val stack: Stack = new Stack()

    /**
     * Creates an action to dispatch a list of errors to the fiber's uncaught
     * error handler.
     *
     * @param errors  The effectfully produced list of errors, in reverse order.
     */
    final def dispatchErrors(errors: IO[List[Throwable]]): IO[Unit] =
      errors.flatMap(errors =>
        // Each error produced by a finalizer must be handled using the
        // context's unhandled exception handler:
        errors.reverse.map(unhandled).foldLeft(IO.unit)(_ *> _)
      )

    /**
     * Catches an exception, returning a (possibly null) finalizer action that
     * must be executed. The stack will be empty in the sole case the exception
     * was not caught by any exception handler.
     *
     * @param err   The exception that is being thrown.
     */
    final def catchException(err: Throwable): IO[List[Throwable]] = {
      var finalizer: IO[List[Throwable]] = null
      var body: BracketResult[Any] = null

      var caught = false

      // Unwind the stack, looking for exception handlers and coalescing
      // finalizers.
      while (!caught && !stack.isEmpty()) {
        stack.pop() match {
          case `Catcher` => caught = true
          case f : Finalizer =>
            if (body == null) body = BracketResult.Failed(err)

            val currentFinalizer: IO[List[Throwable]] = f.finalizer(body).attempt.map {
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
    final def interruptStack(error: Throwable): IO[List[Throwable]] = {
      // Use null to achieve zero allocs for the common case of no finalizers:
      var finalizer: IO[List[Throwable]] = null

      if (!stack.isEmpty()) {
        // Any finalizers will require BracketResult. Here we fake lazy evaluation
        // to eliminate unnecessary allocation:
        var body: BracketResult[Any] = null

        while (!stack.isEmpty()) {
          // Peel off all the finalizers, composing them into a single finalizer
          // that produces a possibly empty list of errors that occurred when
          // executing the finalizers. The order of errors is outer-to-inner
          // (reverse chronological).
          stack.pop() match {
            case f : Finalizer =>
              if (body == null) {
                // Lazy initialization of body:
                body = BracketResult.Interrupted(error)
              }

              val currentFinalizer = f.finalizer(body).attempt.map {
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
    final def evalSync(io0: IO[_]): Unit = {
      try {
        val maxopcount = YieldMaxOpCount

        // Local variables. Do NOT accidentally capture any of these in a closure,
        // or Scala will wrap them in ObjectRef and performance will plummet.
        var result  : Try[Any]  = null
        var curIo   : IO[Any]   = io0.asInstanceOf[IO[Any]]
        var eval    : Boolean   = true
        var opcount : Int       = 0

        while (eval) {
          (curIo.tag : @switch) match {
            case IO.Tags.FlatMap =>
              val io = curIo.asInstanceOf[IO.FlatMap[Any, Any]]

              val nested = io.io

              // A mini interpreter for the left side of FlatMap that evaluates
              // anything that is 1-hop away. This eliminates heap usage for the
              // happy path.
              (nested.tag : @switch) match {
                case IO.Tags.Point =>
                  val io2 = nested.asInstanceOf[IO.Point[Any]]

                  curIo = io.flatMapper(io2.value())

                case IO.Tags.Strict =>
                  val io2 = nested.asInstanceOf[IO.Strict[Any]]

                  curIo = io.flatMapper(io2.value)

                case IO.Tags.SyncEffect =>
                  val io2 = nested.asInstanceOf[IO.SyncEffect[Any]]

                  try {
                    curIo = io.flatMapper(io2.effect())
                  } catch {
                    case t : Throwable if (nonFatal(t)) =>
                      curIo = IO.Fail(t)
                  }

                case _ =>
                  // Fallback case. We couldn't evaluate the LHS so we have to
                  // use the stack:
                  curIo = nested

                  stack.push(io.flatMapper)
              }

            case IO.Tags.Point =>
              val io = curIo.asInstanceOf[IO.Point[Any]]

              val value = io.value()

              curIo = nextInstr(value, stack)

              if (curIo == null) {
                eval   = false
                result = \/-(value)
              }

            case IO.Tags.Strict =>
              val io = curIo.asInstanceOf[IO.Strict[Any]]

              val value = io.value

              curIo = nextInstr(value, stack)

              if (curIo == null) {
                eval   = false
                result = \/-(value)
              }

            case IO.Tags.SyncEffect =>
              val io = curIo.asInstanceOf[IO.SyncEffect[Any]]

              try {
                val value = io.effect()

                curIo = nextInstr(io.effect(), stack)

                if (curIo == null) {
                  eval   = false
                  result = \/-(value)
                }
              } catch {
                case t : Throwable if (nonFatal(t)) =>
                  curIo = IO.Fail(t)
              }

            case IO.Tags.Fail =>
              val io = curIo.asInstanceOf[IO.Fail[Any]]

              val error = io.failure

              val finalizer = catchException(error)

              if (stack.isEmpty()) {
                // Exception not caught, stack is empty:
                if (finalizer == null) {
                  // No finalizer, so immediately produce the error.
                  eval   = false
                  result = -\/(error)
                } else {
                  // We have finalizers to run. We'll resume executing with the
                  // uncaught failure after we have executed all the finalizers:
                  val reported  = dispatchErrors(finalizer)
                  val completer = IO.fail[Any](error)

                  curIo = (reported *> completer).uninterruptibly
                }
              } else {
                val value: Try[Any] = -\/(error)

                // Exception caught:
                if (finalizer == null) {
                  // No finalizer to run:
                  curIo = nextInstr(value, stack)

                  if (curIo == null) {
                    eval   = false
                    result = \/-(value)
                  }
                } else {
                  // Must run finalizer first:
                  val reported  = dispatchErrors(finalizer)
                  val completer = IO.now[Any](\/-(value))

                  curIo = (reported *> completer).uninterruptibly
                }
              }

            case IO.Tags.AsyncEffect =>
              val io = curIo.asInstanceOf[IO.AsyncEffect[Any]]

              try {
                val id = enterAsyncStart()

                try {
                  io.register(resumeAsync(_)) match {
                    case AsyncReturn.Now(value) =>
                      // Value returned synchronously, callback will never be
                      // invoked. Attempt resumption now:
                      if (resumeAsync()) {
                        curIo = nextInstr(value, stack)

                        if (curIo == null) {
                          eval   = false
                          result = \/-(value)
                        }
                      } else {
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
              } catch {
                // The async effect threw an exception, so we'll never get
                // any more information. Treat it as a failure & resume:
                case t : Throwable if (nonFatal(t)) =>
                  if (resumeAsync()) {
                    curIo = IO.Fail(t)
                  } else {
                    eval = false
                  }
              }

            case IO.Tags.Attempt =>
              val io = curIo.asInstanceOf[IO.Attempt[Any]]

              curIo = io.value

              stack.push(Catcher)

            case IO.Tags.Fork =>
              val io = curIo.asInstanceOf[IO.Fork[Any]]

              val handler = Maybe.toOption(io.handler) match {
                case None => unhandled
                case Some(v) => v
              }

              val value: FiberContext[Any] = fork(io.value, handler)

              supervise(value)

              curIo = nextInstr(value : Fiber[Any], stack)

              if (curIo == null) {
                eval   = false
                result = \/-(value)
              }

            case IO.Tags.Race =>
              val io = curIo.asInstanceOf[IO.Race[Any, Any, Any]]

              curIo = raceWith(unhandled, io.left, io.right, io.finish)

            case IO.Tags.Suspend =>
              val io = curIo.asInstanceOf[IO.Suspend[Any]]

              curIo = io.value()

            case IO.Tags.Bracket =>
              val io = curIo.asInstanceOf[IO.Bracket[Any, Any]]

              val ref = new AtomicReference[Any]()

              val finalizer = Finalizer(rez =>
                IO.suspend(if (ref.get != null) io.release(rez, ref.get) else IO.unit ))

              stack.push(finalizer)

              // TODO: Optimize, especially for special case of io.acquire ==
              // IO.unit (ensuring)
              curIo =
                for {
                  a <- (for {
                         a <- io.acquire
                         _ <- IO.sync(ref.set(a))
                       } yield a).uninterruptibly
                  b <- io.use(a)
                  _ <- (io.release(BracketResult.Completed(b), a) <* IO.sync(ref.set(null))).uninterruptibly
                } yield b

            case IO.Tags.Uninterruptible =>
              val io = curIo.asInstanceOf[IO.Uninterruptible[Any]]

              curIo = IO.absolve(for {
                _ <- enterNonInterruptible
                v <- io.io.attempt
                _ <- exitNonInterruptible
              } yield v)

            case IO.Tags.Sleep =>
              val io = curIo.asInstanceOf[IO.Sleep]

              curIo = IO.AsyncEffect { callback =>
                rts.schedule(callback(SuccessUnit.asInstanceOf[Try[Any]]), io.duration).asInstanceOf[AsyncReturn[Any]]
              }

            case IO.Tags.Supervise =>
              val io = curIo.asInstanceOf[IO.Supervise[Any]]

              curIo =
                for {
                  _ <- enterSupervision
                  v <- io.value.ensuring(exitSupervision(io.error))
                } yield v
          }

          opcount = opcount + 1

          if (eval) {
            // Check to see if the fiber needs to be interrupted or cooperatively
            // yield to other fibers:
            shouldDie match {
              case None =>
                // Fiber does not need to be interrupted, but might need to yield:
                if (opcount == maxopcount) {
                  // Cooperatively yield to other fibers currently suspended.
                  // FIXME: Replace with the new design.
                  eval = false

                  opcount = 0

                  rts.submit(evalSync(curIo))
                }

              case Some(t) =>
                // Fiber is being interrupted. Grab all finalizers from the
                // stack to ensure they are run:
                val finalizer = interruptStack(t)

                if (finalizer == null) {
                  // No finalizers, simply produce error:
                  eval    = false
                  result  = -\/(t)
                } else {
                  // Must run finalizers first before failing:
                  val reported  = dispatchErrors(finalizer)
                  val completer = IO.fail[Any](t)

                  curIo = (reported *> completer).uninterruptibly
                }
            }
          }
        }

        if (result != null) {
          done(result.asInstanceOf[Try[A]])
        }
      } catch {
        // Catastrophic error handler. Any error thrown inside the interpreter is
        // either a bug in the interpreter or a bug in the user's code. Let the
        // fiber die but attempt finalization & report errors.
        case t : Throwable if (nonFatal(t)) =>
          val handled = unhandled(t).attempt.map {
            case -\/(t) => t :: Nil
            case _ => Nil
          }

          val finalizer0 = interruptStack(t)

          val finalizer =
            if (finalizer0 == null) handled
            else finalizer0.flatMap(first => handled.map(last => first ::: last))

          val reported  = dispatchErrors(finalizer)
          val completer = IO.fail[Any](t)

          val io = (reported *> completer).uninterruptibly

          // Run on thread pool to avoid growing stack. TODO: Run here by using
          // another top-level loop.
          rts.submit(evalSync(io))
      }
    }

    final def fork[A](io: IO[A], handler: Throwable => IO[Unit]): FiberContext[A] = {
      val context = new FiberContext[A](rts, handler)

      rts.submit(context.evalSync(io))

      context
    }

    /**
     * Resumes a synchronous evaluation given the newly produced value.
     *
     * @param value The value which will be used to resume the sync evaluation.
     */
    private final def resumeEvalSync(value: Try[Any]): Unit = {
      def continueWithValue(v: Any, value: Try[Any]): Unit = {
        val io = nextInstr(v, stack)

        if (io == null) done(value.asInstanceOf[Try[A]])
        else evalSync(io)
      }

      value match {
        case \/-(v) =>
          // Async produced a value:
          continueWithValue(v, value)

        case -\/(t) =>
          // Async produced an error:
          val finalizer = catchException(t)

          if (finalizer == null) {
            if (stack.isEmpty()) done(value.asInstanceOf[Try[A]])
            else continueWithValue(value, \/-(value))
          } else {
            val reported  = dispatchErrors(finalizer)
            val completer = if (stack.isEmpty()) IO.fail(t) else IO.now(\/-(value))

            evalSync((reported *> completer).uninterruptibly)
          }
      }
    }

    /**
     * Resumes an asynchronous computation.
     *
     * @param value     The value produced by the asynchronous computation.
     */
    private final def resumeAsync[A](value: Try[Any]): Unit = {
      if (resumeAsync()) {
        // TODO: CPS transform
        if (this.reentrancy > MaxResumptionDepth) {
          rts.submit(resumeEvalSync(value))
        } else resumeEvalSync(value)
      }
    }

    private final def raceWith[A, B, C](unhandled: Throwable => IO[Unit], leftIO: IO[A], rightIO: IO[B], finish: (A, Fiber[B]) \/ (B, Fiber[A]) => IO[C]): IO[C] = {
      import RaceState._

      val left = fork(leftIO, unhandled)
      val right = fork(rightIO, unhandled)

      // TODO: Kill raced fibers if parent is killed?

      val leftWins = (w: A, l: Fiber[B]) => finish(-\/((w, l)))
      val rightWins = (w: B, l: Fiber[A]) => finish(\/-((w, l)))

      IO.flatten(IO.async0[IO[C]] { resume =>
        val state = new AtomicReference[RaceState](Started)

        def callback[A1, B1](other: Fiber[B1], finish: (A1, Fiber[B1]) => IO[C]): Try[A1] => Unit = (t0: Try[A1]) => {
          val tryA = t0.asInstanceOf[Try[A1]]

          var loop = true
          var action: () => Unit = null

          while (loop) {
            val oldStatus = state.get

            val newState = oldStatus match {
              case Finished => oldStatus
              case OtherFailed =>
                tryA match {
                  case -\/(error) =>
                    action = () => {
                      resume(-\/(error))
                    }
                    Finished
                  case \/-(a) =>
                    action = () => {
                      resume(\/-(finish(a, other)))
                    }
                    Finished
                }
              case Started =>
                tryA match {
                  case -\/(_) => OtherFailed
                  case \/-(a) =>
                    action = () => {
                      resume(\/-(finish(a, other)))
                    }
                    Finished
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

        if (canceler == null) AsyncReturn.later[IO[C]]
        else AsyncReturn.maybeLater(canceler)
      })
    }

    final def interrupt(t: Throwable): IO[Unit] = IO.async0(kill1(t, _))

    final def join: IO[A] = IO.async0(join1(_))

    final def enterSupervision: IO[Unit] = IO.sync {
      supervising += 1

      def newWeakSet[A]: Set[A] = Collections.newSetFromMap[A](new WeakHashMap[A, java.lang.Boolean]())

      val set = newWeakSet[FiberContext[_]]

      supervised = set :: supervised
    }

    final def supervise(child: FiberContext[_]): Unit = {
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
    final def resumeAsync(): Boolean = {
      val oldStatus = status.get

      oldStatus match {
        case AsyncRegion(0, 1, _, joiners, killers) =>
          // No more resumptions are left!
          if (!status.compareAndSet(oldStatus, Executing(joiners, killers))) resumeAsync()
          else true

        case AsyncRegion(reentrancy, resume, _, joiners, killers) =>
          if (!status.compareAndSet(oldStatus, AsyncRegion(reentrancy, resume - 1, None, joiners, killers))) resumeAsync()
          else true

        case _ => false
      }
    }

    final def exitSupervision(e: Throwable): IO[Unit] = IO.flatten(IO.sync {
      supervising -= 1

      var action = IO.unit

      supervised = supervised match {
        case Nil => Nil
        case set :: tail =>
          val iterator = set.iterator()

          while (iterator.hasNext()) {
            val child = iterator.next()

            // TODO: Collect & dispatch errors.
            action = action *> child.interrupt(e)
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

    final def enterNonInterruptible: IO[Unit] = IO.sync { noInterrupt += 1 }

    final def exitNonInterruptible: IO[Unit] = IO.sync { noInterrupt -= 1 }

    final def register(cb: Callback[A]): AsyncReturn[Try[A]] = join0(cb)

    @tailrec
    final def done(v: Try[A]): Unit = {
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
    private final def kill0(t: Throwable, cb: Callback[Unit]): AsyncReturn[Try[Unit]] = {
      killed = true

      val oldStatus = status.get

      oldStatus match {
        case Executing(joiners, killers) =>
          if (!status.compareAndSet(oldStatus, Interrupting(t, joiners, cb :: killers))) kill0(t, cb)
          else AsyncReturn.later[Try[Unit]]

        case Interrupting(t, joiners, killers) =>
          if (!status.compareAndSet(oldStatus, Interrupting(t, joiners, cb :: killers))) kill0(t, cb)
          else AsyncReturn.later[Try[Unit]]

        case AsyncRegion(_, resume, cancelOpt, joiners, killers) if (resume > 0 && noInterrupt == 0) =>
          val v = -\/[Throwable, A](t)

          if (!status.compareAndSet(oldStatus, Done(v))) kill0(t, cb)
          else {
            // We interrupted async before it could resume. Now we have to
            // cancel the computation, if possible, and handle any finalizers.
            cancelOpt match {
              case None =>
              case Some(cancel) =>
                try cancel(t) catch { case t : Throwable if (nonFatal(t)) => /* TODO: Don't throw away? */ }
            }

            val finalizer = interruptStack(t)

            if (finalizer != null) {
              fork(dispatchErrors(finalizer), unhandled)
            }

            purgeJoinersKillers(v, joiners, killers)

            AsyncReturn.now(SuccessUnit)
          }

        case AsyncRegion(_, _, _, joiners, killers) =>
          if (!status.compareAndSet(oldStatus, Interrupting(t, joiners, cb :: killers))) kill0(t, cb)
          else AsyncReturn.later[Try[Unit]]

        case Done(_) => AsyncReturn.now(SuccessUnit)
      }
    }

    private final def kill1(t: Throwable, cb: Callback[Unit]): AsyncReturn[Unit] =
      mapExtract(kill0(t, cb))

    @tailrec
    private final def join0(cb: Callback[A]): AsyncReturn[Try[A]] = {
      val oldStatus = status.get

      oldStatus match {
        case Executing(joiners, killers) =>
          if (!status.compareAndSet(oldStatus, Executing(cb :: joiners, killers))) join0(cb)
          else AsyncReturn.later[Try[A]]

        case Interrupting(t, joiners, killers) =>
          if (!status.compareAndSet(oldStatus, Interrupting(t, cb :: joiners, killers))) join0(cb)
          else AsyncReturn.later[Try[A]]

        case AsyncRegion(reenter, resume, cancel, joiners, killers) =>
          if (!status.compareAndSet(oldStatus, AsyncRegion(reenter, resume, cancel, cb :: joiners, killers))) join0(cb)
          else AsyncReturn.later[Try[A]]

        case Done(v) => AsyncReturn.now(v)
      }
    }

    private final def join1(cb: Callback[A]): AsyncReturn[A] = mapExtract(join0(cb))

    private final def mapExtract[B](m: AsyncReturn[Try[B]]): AsyncReturn[B] = m match {
      case AsyncReturn.Now(v) => AsyncReturn.Now(extract(v))
      case AsyncReturn.MaybeLater(c) => AsyncReturn.MaybeLater[B](c)
      case _ => AsyncReturn.later[B]
    }

    private def purgeJoinersKillers(v: Try[A], joiners: List[Callback[A]], killers: List[Callback[Unit]]): Unit = {
      // FIXME: Put all but one of these (first joiner?) on the thread pool.
      killers.reverse.foreach(_.apply(SuccessUnit))
      joiners.reverse.foreach(_.apply(v))
    }
  }

  sealed trait FiberStatus[A]
  object FiberStatus {
    final case class Executing[A](joiners: List[Callback[A]], killers: List[Callback[Unit]]) extends FiberStatus[A]
    final case class Interrupting[A](error: Throwable, joiners: List[Callback[A]], killers: List[Callback[Unit]]) extends FiberStatus[A]
    final case class AsyncRegion[A](reentrancy: Int, resume: Int, cancel: Option[Throwable => Unit], joiners: List[Callback[A]], killers: List[Callback[Unit]]) extends FiberStatus[A]
    final case class Done[A](value: Try[A]) extends FiberStatus[A]

    def Initial[A] = Executing[A](Nil, Nil)
  }

  val SuccessUnit: Try[Unit] = \/-(())
  val NonCanceler: Throwable => Unit = (t: Throwable) => ()
}
