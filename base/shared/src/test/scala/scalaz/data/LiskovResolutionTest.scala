package scalaz.data

import scalaz.Prelude._

class LiskovResolutionTest {
  implicitly[Liskov[Nothing, Any, Int, AnyVal]]
  implicitly[Liskov[Int, Any, Int, AnyVal]]
  implicitly[Liskov[Int, AnyVal, Int, AnyVal]]
}
