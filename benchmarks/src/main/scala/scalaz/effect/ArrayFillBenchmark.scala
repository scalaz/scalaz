package scalaz.effect

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import IOBenchmarks.unsafePerformIO

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class ArrayFillBenchmarks {
  @Param(Array("10000"))
  var size: Int = _

  def createTestArray: Array[Int] = (1 to size).toArray.reverse

  @Benchmark
  def scalazArrayFill() = {
    def arrayFill(array: Array[Int]) = {
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
}
