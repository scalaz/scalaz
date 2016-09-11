package scalaz
package benchmarks

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode}

@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class FunctionsCompose {
  import FunctionsCompose._

  @Benchmark def baseline   = run(_baseline)
  @Benchmark def scala      = run(_andThen)
  @Benchmark def scalaz     = run(_composer)
}

object FunctionsCompose extends meta.ComposerSyntax {
  val f: Int => Double = _ + 0.5
  val g: Double => String = _.toString
  val h: String => Int = _.toDouble.toInt + 10

  def _baseline: Int => Int = a => h(g(f(h(g(f(h(g(f(h(g(f(a))))))))))))
  def _andThen: Int => Int = (f andThen g andThen h andThen f andThen g andThen h andThen f andThen
                              g andThen h andThen f andThen g andThen h)
  def _composer: Int => Int = (f, g, h, f, g, h, f, g, h, f, g, h).compose

  def run(f: Int => Int): Int = f(42)
}
