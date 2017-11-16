// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz.effect

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.concurrent.Await

import IOBenchmarks._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class IOLeftBindBenchmark {
  @Param(Array("10000"))
  var size: Int = _

  @Param(Array("100"))
  var depth: Int = _

  @Benchmark
  def thunkLeftBindBenchmark(): Int = {
    def loop(i: Int): Thunk[Int] =
      if (i % depth == 0) Thunk(i + 1).flatMap(loop)
      else if (i < size) loop(i + 1).flatMap(i => Thunk(i))
      else Thunk(i)

    Thunk(0).unsafePerformIO()
  }

  @Benchmark
  def futureLeftBindBenchmark(): Int = {
    import scala.concurrent.Future
    import scala.concurrent.duration.Duration.Inf

    def loop(i: Int): Future[Int] =
      if (i % depth == 0) Future(i + 1).flatMap(loop)
      else if (i < size) loop(i + 1).flatMap(i => Future(i))
      else Future(i)

    Await.result(Future(0).flatMap(loop), Inf)
  }

  @Benchmark
  def monixLeftBindBenchmark(): Int = {
    import monix.eval.Task

    def loop(i: Int): Task[Int] =
      if (i % depth == 0) Task.eval(i + 1).flatMap(loop)
      else if (i < size) loop(i + 1).flatMap(i => Task.eval(i))
      else Task.eval(i)

    Task.eval(0).flatMap(loop).runSyncMaybe.right.get
  }

  @Benchmark
  def scalazLeftBindBenchmark(): Int = {
    def loop(i: Int): IO[Int] =
      if (i % depth == 0) IO(i + 1).flatMap(loop)
      else if (i < size) loop(i + 1).flatMap(i => IO(i))
      else IO(i)

    unsafePerformIO(IO(0).flatMap(loop))
  }

  @Benchmark
  def catsLeftBindBenchmark(): BigInt = {
    import cats.effect._

    def loop(i: Int): IO[Int] =
      if (i % depth == 0) IO(i + 1).flatMap(loop)
      else if (i < size) loop(i + 1).flatMap(i => IO(i))
      else IO(i)

    IO(0).flatMap(loop).unsafeRunSync
  }
}
