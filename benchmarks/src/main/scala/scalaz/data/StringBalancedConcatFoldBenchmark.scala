package scalaz
package data

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.{ Array, List, Nil, Predef }
import Predef.{ wrapString, String }

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Xms2g", "-Xmx2g"))
class StringBalancedConcatFoldBenchmark {
  @Param(Array("15"))
  var size: Int = _

  final val testString: String = "testString" * 5

  @Benchmark
  def listStringBalancedConcatFoldBenchmark(): String = {
    def loop(i: Int, strs: List[String]): List[String] =
      if (i > 0) loop(i - 1, strs ++ strs)
      else strs
    loop(size, (testString :: Nil)).mkString("")
  }

  @Benchmark
  def cordBalancedConcatFoldBenchmark(): String = {
    def loop(i: Int, cord: scalaz.data.Cord): scalaz.data.Cord =
      if (i > 0) loop(i - 1, scalaz.data.Cord.concat(cord, cord))
      else cord
    scalaz.data.Cord.toString(loop(size, scalaz.data.Cord(testString)))
  }

  @Benchmark
  def stringBuilderBalancedConcatFoldBenchmark(): String = {
    def loop(i: Int, sb: java.lang.StringBuilder): Unit =
      if (i > 0) {
        loop(i - 1, sb.append(sb.toString))
      } else ()
    val sb = new java.lang.StringBuilder()
    loop(size, sb)
    sb.toString
  }
}
