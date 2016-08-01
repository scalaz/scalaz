package scalaz
package benchmarks

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode}

@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class OpticsScalaz {
  import Optics._
  import Scalaz._

  @Benchmark def lens   = _lens
  @Benchmark def get    = _telephoto.get(init)
  @Benchmark def set    = _telephoto.set(init, 21)
  @Benchmark def modify = _telephoto.modify(init)(_ / 2)
}
