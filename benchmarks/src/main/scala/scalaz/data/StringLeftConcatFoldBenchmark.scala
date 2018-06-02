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
class StringLeftConcatFoldBenchmark {
  @Param(Array("10000"))
  var size: Int = _

  final val testString: String = "testString" * 5

  @Benchmark
  def listStringLeftConcatFoldBenchmark(): String = {
    def loop(i: Int, strs: List[String]): List[String] =
      if (i > 0) loop(i - 1, testString :: strs)
      else strs
    loop(size, Nil).mkString("")
  }

  @Benchmark
  def cordLeftConcatFoldBenchmark(): String = {
    def loop(i: Int, cord: scalaz.data.Cord): scalaz.data.Cord =
      if (i > 0) loop(i - 1, scalaz.data.Cord.cons(testString, cord))
      else cord
    scalaz.data.Cord.fold(loop(size, scalaz.data.Cord.empty))
  }

  @Benchmark
  def stringBuilderLeftConcatFoldBenchmark(): String = {
    def loop(i: Int, sb: java.lang.StringBuilder): Unit =
      if (i > 0) loop(i - 1, sb.insert(0, testString))
      else ()
    val sb = new java.lang.StringBuilder()
    loop(size, sb)
    sb.toString
  }

}
