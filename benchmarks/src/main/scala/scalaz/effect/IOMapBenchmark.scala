// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz.effect

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.concurrent.Await
import scala.annotation.tailrec

import IOBenchmarks._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class IOMapBenchmark {
  @Param(Array("500"))
  var depth: Int = _

  @Benchmark
  def thunkMap(): BigInt = {
    @tailrec
    def sumTo(t: Thunk[BigInt], n: Int): Thunk[BigInt] =
      if (n <= 1) t
      else sumTo(t.map(_ + n), n - 1)

    sumTo(Thunk(0), depth).unsafePerformIO()
  }

  @Benchmark
  def futureMap(): BigInt = {
    import scala.concurrent.Future
    import scala.concurrent.duration.Duration.Inf

    @tailrec
    def sumTo(t: Future[BigInt], n: Int): Future[BigInt] =
      if (n <= 1) t
      else sumTo(t.map(_ + n), n - 1)

    Await.result(sumTo(Future(0), depth), Inf)
  }

  @Benchmark
  def monixMap(): BigInt = {
    import monix.eval.Task

    @tailrec
    def sumTo(t: Task[BigInt], n: Int): Task[BigInt] =
      if (n <= 1) t
      else sumTo(t.map(_ + n), n - 1)

    sumTo(Task.eval(0), depth).runSyncMaybe.right.get
  }

  @Benchmark
  def scalazMap(): BigInt = {
    @tailrec
    def sumTo(t: IO[BigInt], n: Int): IO[BigInt] =
      if (n <= 1) t
      else sumTo(t.map(_ + n), n - 1)

    unsafePerformIO(sumTo(IO.point(0), depth))
  }

  @Benchmark
  def catsMap(): BigInt = {
    import cats.effect._

    @tailrec
    def sumTo(t: IO[BigInt], n: Int): IO[BigInt] =
      if (n <= 1) t
      else sumTo(t.map(_ + n), n - 1)

    sumTo(IO(0), depth).unsafeRunSync()
  }
}
