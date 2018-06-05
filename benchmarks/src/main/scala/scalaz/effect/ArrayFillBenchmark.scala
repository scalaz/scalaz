package scalaz.effect

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scala.{ Array, Boolean, Int, Unit }
import scala.collection.immutable.Range
import scala.Predef.{ genericArrayOps, genericWrapArray }

import scalaz.Void

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class ArrayFillBenchmarks {
  @Param(Array("10000"))
  var size: Int = _

  def createTestArray: Array[Int] = Range.inclusive(1, size).toArray.reverse

  @Benchmark
  def scalazArrayFill() = {
    import IOBenchmarks.unsafePerformIO

    def arrayFill(array: Array[Int]): KleisliIO[Void, Int, Int] = {
      val condition = KleisliIO.lift[Void, Int, Boolean]((i: Int) => i < array.length)

      KleisliIO.whileDo(condition)(KleisliIO.impureVoid[Int, Int] { (i: Int) =>
        array.update(i, i)

        i + 1
      })
    }

    unsafePerformIO(
      for {
        array <- IO.sync(createTestArray)
        _     <- arrayFill(array)(0)
      } yield ()
    )
  }
  @Benchmark
  def catsArrayFill() = {
    import cats.effect.IO

    def arrayFill(array: Array[Int])(i: Int): IO[Unit] =
      if (i >= array.length) IO.unit
      else IO(array.update(i, i)).flatMap(_ => arrayFill(array)(i + 1))

    (for {
      array <- IO(createTestArray)
      _     <- arrayFill(array)(0)
    } yield ()).unsafeRunSync()
  }
  @Benchmark
  def monixArrayFill() = {
    import monix.eval.Task
    import IOBenchmarks.monixScheduler

    def arrayFill(array: Array[Int])(i: Int): Task[Unit] =
      if (i >= array.length) Task.unit
      else Task.eval(array.update(i, i)).flatMap(_ => arrayFill(array)(i + 1))

    (for {
      array <- Task.eval(createTestArray)
      _     <- arrayFill(array)(0)
    } yield ()).toIO.unsafeRunSync()
  }
}
