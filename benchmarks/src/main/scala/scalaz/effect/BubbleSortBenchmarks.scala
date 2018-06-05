package scalaz.effect

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scala.{ Array, Int, Unit }
import scala.collection.immutable.Range
import scala.Predef.{ genericArrayOps, genericWrapArray }

import IOBenchmarks.unsafePerformIO

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class BubbleSortBenchmarks {
  @Param(Array("1000"))
  var size: Int = _

  def createTestArray: Array[Int] = Range.inclusive(1, size).toArray.reverse
  def assertSorted(array: Array[Int]): Unit =
    if (!array.sorted.sameElements(array)) {
      throw new Exception("Array not correctly sorted")
    }

  @Benchmark
  def scalazBubbleSort() = {
    import ScalazIOArray._

    unsafePerformIO(
      for {
        array <- IO.sync(createTestArray)
        _     <- bubbleSort[Int](_ <= _)(array)
        _     <- IO.sync(assertSorted(array))
      } yield ()
    )
  }
  @Benchmark
  def catsBubbleSort() = {
    import CatsIOArray._
    import cats.effect.IO

    (for {
      array <- IO(createTestArray)
      _     <- bubbleSort[Int](_ <= _)(array)
      _     <- IO(assertSorted(array))
    } yield ()).unsafeRunSync()
  }
  @Benchmark
  def monixBubbleSort() = {
    import MonixIOArray._
    import monix.eval.Task
    import IOBenchmarks.monixScheduler

    (for {
      array <- Task.eval(createTestArray)
      _     <- bubbleSort[Int](_ <= _)(array)
      _     <- Task.eval(assertSorted(array))
    } yield ()).toIO.unsafeRunSync()
  }
}
