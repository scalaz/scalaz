package scalaz.example


import org.specs.{Sugar, Specification}

class ExamplesTest extends Specification with Sugar {
  "run" in {
    Example.run must_== () 
  }
}
