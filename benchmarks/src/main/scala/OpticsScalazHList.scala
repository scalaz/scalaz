package scalaz
package benchmarks

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode}

@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class OpticsScalazHList {
  import Optics._
  import Scalaz._

  @Benchmark def get    = _telephotoHList.get(init)
  @Benchmark def set    = _telephotoHList.set(init, 21)
  @Benchmark def modify = _telephotoHList.modify(init)(_ / 2)
}
