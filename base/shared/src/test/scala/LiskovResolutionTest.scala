package scalaz

import scala.{ Any, AnyVal, Nothing }

class LiskovResolutionTest {
  implicitly[Liskov[Nothing, Any, Int, AnyVal]]
  implicitly[Liskov[Int, Any, Int, AnyVal]]
  implicitly[Liskov[Int, AnyVal, Int, AnyVal]]
}
