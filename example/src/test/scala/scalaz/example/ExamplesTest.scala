package scalaz.example


import org.specs.{Sugar, Specification}

object ExamplesTest extends Specification with Sugar {
  "run" in {
    Example.run must_== ()
  }
}
