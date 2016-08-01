package scalaz
package benchmarks

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode}

@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class OpticsMonocle {
  import Optics._
  import Monocle._

  @Benchmark def lens   = _lens
  @Benchmark def get    = _compose.get(init)
  @Benchmark def set    = _compose.set(21)(init)
  @Benchmark def modify = _compose.modify(_ / 2)(init)
}

