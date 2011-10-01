package scalaz

import org.specs.{Sugar, Specification}

import concurrent._
import Scalaz._

class PromiseTest extends Specification with Sugar {
  "exception in flatMapped promise is propagated" in {
    def !!! = sys.error("!!!")
    def flatMapError: Promise[Int] = promise(1).flatMap(_ => promise[Int](!!!))
    flatMapError.get must throwA[RuntimeException]

    def sequenced = Seq[Promise[Int]](promise(1), promise(!!!)).sequence
    sequenced.get must throwA[RuntimeException]
  }
}