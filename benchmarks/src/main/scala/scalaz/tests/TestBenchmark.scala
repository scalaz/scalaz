package scalaz
package tests

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.Array
import scalaz.Predef._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Fork(value = 5, jvmArgsAppend = Array("-Xms2g", "-Xmx2g"))
@Warmup(iterations = 0, batchSize = 1)
@Measurement(iterations = 1, batchSize = 1, time = 1, timeUnit = TimeUnit.MILLISECONDS)
class TestBenchmark {
  @Benchmark
  def allTests(): Unit =
    TestMain.main(Array.empty)
}
