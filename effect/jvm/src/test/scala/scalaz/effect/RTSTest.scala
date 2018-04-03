// Copyright (C) 2017-2018 John A. De Goes. All rights reserved.
package scalaz.effect

import scala.concurrent.duration._

import org.specs2.concurrent.ExecutionEnv
import org.specs2.Specification
import org.specs2.specification.AroundTimeout

import scalaz.data.Disjunction._

import scalaz.effect.Errors.UnhandledError

class RTSSpec(implicit ee : ExecutionEnv) extends Specification
    with AroundTimeout
    with RTS {

  override def defaultHandler[E]: Throwable => IO[E, Unit] = _ => IO.unit[E]

  def is = s2"""
  RTS synchronous correctness
    evaluation of point                     $testPoint
    point must be lazy                      $testPointIsLazy
    now must be eager                       $testNowIsEager
    suspend must be lazy                    $testSuspendIsLazy
    suspend must be evaluatable             $testSuspendIsEvaluatable
    point, bind, map                        $testSyncEvalLoop
    sync effect                             $testEvalOfSyncEffect
    deep effects                            $testEvalOfDeepSyncEffect

  RTS failure
    error in sync effect                    $testEvalOfAttemptOfSyncEffectError
    attempt . fail                          $testEvalOfAttemptOfFail
    deep attempt sync effect error          $testAttemptOfDeepSyncEffectError
    deep attempt fail error                 $testAttemptOfDeepFailError
    uncaught fail                           $testEvalOfUncaughtFail
    uncaught sync effect error              $testEvalOfUncaughtThrownSyncEffect
    deep uncaught sync effect error         $testEvalOfDeepUncaughtThrownSyncEffect
    deep uncaught fail                      $testEvalOfDeepUncaughtFail

  RTS bracket
    fail ensuring                           $testEvalOfFailEnsuring
    fail on error                           $testEvalOfFailOnError
    finalizer errors not caught             $testErrorInFinalizerCannotBeCaught
    finalizer errors reported               ${upTo(1.second)(testErrorInFinalizerIsReported)}
    bracket result is usage result          $testFiberResultIsUsageResult
    error in just acquisition               $testBracketErrorInAcquisition
    error in just release                   $testBracketErrorInRelease
    error in just usage                     $testBracketErrorInUsage
    rethrown caught error in acquisition    $testBracketRethrownCaughtErrorInAcquisition
    rethrown caught error in release        $testBracketRethrownCaughtErrorInRelease
    rethrown caught error in usage          $testBracketRethrownCaughtErrorInUsage
    test eval of async fail                 $testEvalOfAsyncAttemptOfFail

  RTS synchronous stack safety
    deep map of point                       $testDeepMapOfPoint
    deep map of now                         $testDeepMapOfNow
    deep map of sync effect                 $testDeepMapOfSyncEffectIsStackSafe
    deep attempt                            $testDeepAttemptIsStackSafe

  RTS asynchronous stack safety
    deep bind of async chain                $testDeepBindOfAsyncChainIsStackSafe

  RTS asynchronous correctness
    simple async must return                $testAsyncEffectReturns
    sleep 0 must return                     ${upTo(1.second)(testSleepZeroReturns)}

  RTS concurrency correctness
    shallow fork/join identity              $testForkJoinIsId
    deep fork/join identity                 $testDeepForkJoinIsId
    interrupt of never                      ${upTo(1.second)(testNeverIsInterruptible)}
    race of value & never                   ${upTo(1.second)(testRaceOfValueNever)}

  """

  def testPoint = {
    unsafePerformIO(IO.point(1)) must_=== 1
  }

  def testPointIsLazy = {
    IO.point(throw new Error("Not lazy")) must not (throwA[Throwable])
  }

  def testNowIsEager = {
    (IO.now(throw new Error("Eager"))) must (throwA[Error])
  }

  def testSuspendIsLazy = {
    IO.suspend(throw new Error("Eager")) must not (throwA[Throwable])
  }

  def testSuspendIsEvaluatable = {
    unsafePerformIO(IO.suspend(IO.point[Throwable, Int](42))) must_=== 42
  }

  def testSyncEvalLoop = {
    def fibIo(n: Int): IO[Throwable, BigInt] =
      if (n <= 1) IO.point(n) else for {
        a <- fibIo(n - 1)
        b <- fibIo(n - 2)
      } yield a + b

    unsafePerformIO(fibIo(10)) must_=== fib(10)
  }

  def testEvalOfSyncEffect = {
    def sumIo(n: Int): IO[Throwable, Int] =
      if (n <= 0) IO.sync(0)
      else IO.sync(n).flatMap(b => sumIo(n - 1).map(a => a + b))

    unsafePerformIO(sumIo(1000)) must_=== sum(1000)
  }

  def testEvalOfAttemptOfSyncEffectError = {
    unsafePerformIO(IO.partialSync(throw ExampleError).attempt[Throwable]) must_=== -\/(ExampleError)
  }

  def testEvalOfAttemptOfFail = {
    unsafePerformIO(IO.fail[Throwable, Int](ExampleError).attempt[Throwable]) must_=== -\/(ExampleError)

    unsafePerformIO(IO.suspend(IO.suspend(IO.fail[Throwable, Int](ExampleError)).attempt[Throwable])) must_=== -\/(ExampleError)
  }

  def testAttemptOfDeepSyncEffectError = {
    unsafePerformIO(deepErrorEffect(100).attempt[Throwable]) must_=== -\/(ExampleError)
  }

  def testAttemptOfDeepFailError = {
    unsafePerformIO(deepErrorFail(100).attempt[Throwable]) must_=== -\/(ExampleError)
  }

  def testEvalOfUncaughtFail = {
    unsafePerformIO(IO.fail[Throwable, Int](ExampleError)) must (throwA(UnhandledError(ExampleError)))
  }

  def testEvalOfUncaughtThrownSyncEffect = {
    unsafePerformIO(IO.sync[Throwable, Int](throw ExampleError)) must (throwA(ExampleError))
  }

  def testEvalOfDeepUncaughtThrownSyncEffect = {
    unsafePerformIO(deepErrorEffect(100)) must (throwA(UnhandledError(ExampleError)))
  }

  def testEvalOfDeepUncaughtFail = {
    unsafePerformIO(deepErrorEffect(100)) must (throwA(UnhandledError(ExampleError)))
  }

  def testEvalOfFailEnsuring = {
    var finalized = false

    unsafePerformIO(IO.fail[Throwable, Unit](ExampleError).ensuring(IO.sync[Throwable, Unit] { finalized = true; () })) must (throwA(ExampleError))
    finalized must_=== true
  }

  def testEvalOfFailOnError = {
    var finalized = false

    unsafePerformIO(IO.fail[Throwable, Unit](ExampleError).onError(_ => IO.sync[Throwable, Unit] { finalized = true; () })) must (throwA(ExampleError))

    finalized must_=== true
  }

  def testErrorInFinalizerCannotBeCaught = {
    val nested: IO[Throwable, Int] =
      IO.fail[Throwable, Int](ExampleError).ensuring(
        IO.fail[Throwable, Unit](new Error("e2"))).ensuring(
          IO.fail[Throwable, Unit](new Error("e3")))

    unsafePerformIO(nested) must (throwA(UnhandledError(ExampleError)))
  }

  def testErrorInFinalizerIsReported = {
    var reported: Throwable = null

    unsafePerformIO {
      IO.point[Throwable, Int](42).ensuring(IO.fail[Throwable, Unit](ExampleError)).
        fork0(e => IO.sync[Throwable, Unit] { reported = e; () })
    }

    // FIXME: Is this an issue with thread synchronization?
    while (reported == null) Thread.`yield`()

    ((throw reported): Int) must (throwA(UnhandledError(ExampleError)))
  }

  def testFiberResultIsUsageResult = {
    unsafePerformIO(IO.unit.bracket_(IO.unit[Throwable])(IO.point[Throwable, Int](42))) must_=== 42
  }

  def testBracketErrorInAcquisition = {
    unsafePerformIO(IO.fail[Throwable, Unit](ExampleError).bracket_(IO.unit)(IO.unit)) must
      (throwA(UnhandledError(ExampleError)))
  }

  def testBracketErrorInRelease = {
    unsafePerformIO(IO.unit.bracket_(IO.fail[Throwable, Unit](ExampleError))(IO.unit)) must
      (throwA(UnhandledError(ExampleError)))
  }

  def testBracketErrorInUsage = {
    unsafePerformIO(IO.unit.bracket_(IO.unit)(IO.fail[Throwable, Unit](ExampleError))) must
      (throwA(UnhandledError(ExampleError)))
  }

  def testBracketRethrownCaughtErrorInAcquisition = {
    lazy val actual = unsafePerformIO(IO.absolve(IO.fail[Throwable, Unit](ExampleError).bracket_(IO.unit)(IO.unit).attempt[Throwable]))

    actual must (throwA(UnhandledError(ExampleError)))
  }

  def testBracketRethrownCaughtErrorInRelease = {
    lazy val actual = unsafePerformIO(IO.absolve(IO.unit.bracket_(IO.fail[Throwable, Unit](ExampleError))(IO.unit).attempt[Throwable]))

    actual must (throwA(UnhandledError(ExampleError)))
  }

  def testBracketRethrownCaughtErrorInUsage = {
    lazy val actual = unsafePerformIO(IO.absolve(IO.unit.bracket_(IO.unit)(IO.fail[Throwable, Unit](ExampleError)).attempt[Throwable]))

    actual must (throwA(UnhandledError(ExampleError)))
  }

  def testEvalOfAsyncAttemptOfFail = {
    val io1 = IO.unit.bracket_(AsyncUnit)(asyncExampleError[Unit])
    val io2 = AsyncUnit.bracket_(IO.unit)(asyncExampleError[Unit])

    unsafePerformIO(io1) must (throwA(UnhandledError(ExampleError)))
    unsafePerformIO(io2) must (throwA(UnhandledError(ExampleError)))
    unsafePerformIO(IO.absolve(io1.attempt[Throwable])) must (throwA(UnhandledError(ExampleError)))
    unsafePerformIO(IO.absolve(io2.attempt[Throwable])) must (throwA(UnhandledError(ExampleError)))
  }

  def testEvalOfDeepSyncEffect = {
    def incLeft(n: Int, ref: IORef[Int]): IO[Throwable, Int] =
      if (n <= 0) ref.read
      else incLeft(n - 1, ref) <* ref.modify(_ + 1)

    def incRight(n: Int, ref: IORef[Int]): IO[Throwable, Int] =
      if (n <= 0) ref.read
      else ref.modify(_ + 1) *> incRight(n - 1, ref)

    unsafePerformIO(for {
      ref <- IORef(0)
      v   <- incLeft(100, ref)
    } yield v) must_=== 100

    unsafePerformIO(for {
      ref <- IORef(0)
      v   <- incRight(1000, ref)
    } yield v) must_=== 1000
  }

  def testDeepMapOfPoint = {
    unsafePerformIO(deepMapPoint(10000)) must_=== 10000
  }

  def testDeepMapOfNow = {
    unsafePerformIO(deepMapNow(10000)) must_=== 10000
  }

  def testDeepMapOfSyncEffectIsStackSafe = {
    unsafePerformIO(deepMapEffect(10000)) must_=== 10000
  }

  def testDeepAttemptIsStackSafe = {
    unsafePerformIO((0 until 10000).foldLeft(IO.sync[Throwable, Unit](())) { (acc, _) =>
      acc.attempt[Throwable].toUnit
    }) must_=== (())
  }

  def testDeepBindOfAsyncChainIsStackSafe = {
    val result = (0 until 10000).foldLeft(IO.point[Throwable, Int](0)) { (acc, _) =>
      acc.flatMap(n => IO.async[Throwable, Int](_(FiberResult.Completed[Throwable, Int](n + 1))))
    }

    unsafePerformIO(result) must_=== 10000
  }

  def testAsyncEffectReturns = {
    unsafePerformIO(IO.async[Throwable, Int](cb => cb(FiberResult.Completed(42)))) must_=== 42
  }

  def testSleepZeroReturns = {
    unsafePerformIO(IO.sleep(1.nanoseconds)) must_=== ((): Unit)
  }

  def testForkJoinIsId = {
    unsafePerformIO(IO.point[Throwable, Int](42).fork.flatMap(_.join)) must_=== 42
  }

  def testDeepForkJoinIsId = {
    val n = 20

    unsafePerformIO(concurrentFib(n)) must_=== fib(n)
  }

  def testNeverIsInterruptible = {
    val io =
      for {
        fiber <- IO.never[Throwable, Int].fork
        _     <- fiber.interrupt(ExampleError)
      } yield 42

    unsafePerformIO(io) must_=== 42
  }

  def testRaceOfValueNever = {
    unsafePerformIO(IO.point(42).race(IO.never[Throwable, Int])) == 42
  }

  // Utility stuff
  val ExampleError = new Exception("Oh noes!")

  def asyncExampleError[A]: IO[Throwable, A] = IO.async[Throwable, A](_(FiberResult.Failed(ExampleError)))

  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def deepMapPoint(n: Int): IO[Throwable, Int] =
    if (n <= 0) IO.point(n) else IO.point(n - 1).map(_ + 1)

  def deepMapNow(n: Int): IO[Throwable, Int] =
    if (n <= 0) IO.now(n) else IO.now(n - 1).map(_ + 1)

  def deepMapEffect(n: Int): IO[Throwable, Int] =
    if (n <= 0) IO.sync(n) else IO.sync(n - 1).map(_ + 1)

  def deepErrorEffect(n: Int): IO[Throwable, Unit] =
    if (n == 0) IO.partialSync(throw ExampleError)
    else IO.unit *> deepErrorEffect(n - 1)

  def deepErrorFail(n: Int): IO[Throwable, Unit] =
    if (n == 0) IO.fail(ExampleError)
    else IO.unit *> deepErrorFail(n - 1)

  def fib(n: Int): BigInt =
    if (n <= 1) n
    else fib(n - 1) + fib(n - 2)

  def concurrentFib(n: Int): IO[Throwable, BigInt] =
    if (n <= 1) IO.point[Throwable, BigInt](n)
    else for {
      f1 <- concurrentFib(n - 1).fork
      f2 <- concurrentFib(n - 2).fork
      v1 <- f1.join
      v2 <- f2.join
    } yield v1 + v2

  val AsyncUnit = IO.async[Throwable, Unit](_(FiberResult.Completed(())))
}
