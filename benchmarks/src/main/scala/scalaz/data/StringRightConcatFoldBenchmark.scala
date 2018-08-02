package scalaz
package data

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.{ Array, List, Nil }
import scala.Predef.{ wrapString, String }
import scalaz.Predef._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Xms2g", "-Xmx2g"))
class StringRightConcatFoldBenchmark {
  @Param(Array("10000"))
  var size: Int = _

  final val testString: String = "testString" * 5

  @Benchmark
  def listStringRightConcatFoldBenchmark(): String = {
    def loop(i: Int, strs: List[String]): List[String] =
      if (i > 0) loop(i - 1, strs :+ testString)
      else strs
    loop(size, Nil).mkString("")
  }

  @Benchmark
  def cordRightConcatFoldBenchmark(): String = {
    def loop(i: Int, cord: scalaz.data.Cord): scalaz.data.Cord =
      if (i > 0) loop(i - 1, scalaz.data.Cord.snoc(cord, testString))
      else cord
    scalaz.data.Cord.toString(loop(size, scalaz.data.Cord.empty))
  }

  @Benchmark
  def stringBuilderRightConcatFoldBenchmark(): String = {
    def loop(i: Int, sb: java.lang.StringBuilder): Unit =
      if (i > 0) loop(i - 1, sb.append(testString))
      else ()
    val sb = new java.lang.StringBuilder()
    loop(size, sb)
    sb.toString
  }
}
