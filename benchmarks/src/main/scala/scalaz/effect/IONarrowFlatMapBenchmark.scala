// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz.effect

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.concurrent.Await

import IOBenchmarks._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class IONarrowFlatMapBenchmark {
  @Param(Array("10000"))
  var size: Int = _

  @Benchmark
  def thunkNarrowFlatMap(): Int = {
    def loop(i: Int): Thunk[Int] =
      if (i < size) Thunk(i + 1).flatMap(loop)
      else Thunk(i)

    Thunk(0).unsafePerformIO()
  }

  @Benchmark
  def futureNarrowFlatMap(): Int = {
    import scala.concurrent.Future
    import scala.concurrent.duration.Duration.Inf

    def loop(i: Int): Future[Int] =
      if (i < size) Future(i + 1).flatMap(loop)
      else Future(i)

    Await.result(Future(0).flatMap(loop), Inf)
  }

  @Benchmark
  def monixNarrowFlatMap(): Int = {
    import monix.eval.Task

    def loop(i: Int): Task[Int] =
      if (i < size) Task.eval(i + 1).flatMap(loop)
      else Task.eval(i)

    Task.eval(0).flatMap(loop).runSyncMaybe.right.get
  }

  @Benchmark
  def scalazNarrowFlatMap(): Int = {
    def loop(i: Int): IO[Int] =
      if (i < size) IO.point(i + 1).flatMap(loop)
      else IO.point(i)

    unsafePerformIO(IO.point(0).flatMap(loop))
  }

  @Benchmark
  def catsNarrowFlatMap(): Int = {
    import cats.effect._

    def loop(i: Int): IO[Int] =
      if (i < size) IO(i + 1).flatMap(loop)
      else IO(i)

    IO(0).flatMap(loop).unsafeRunSync
  }
}
