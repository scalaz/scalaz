package scalaz
package benchmarks

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode}

@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class OpticsBaseline {
  import Optics._
  import Scalaz._

  @Benchmark def get    = _baseline.get(init)
  @Benchmark def set    = _baseline.set(init, 21)
  @Benchmark def modify = _baseline.modify(init)(_ / 2)
}
