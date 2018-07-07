package scalaz

import scala.{ Any, AnyVal, Int, Nothing }

import Predef._
import prop.Liskov

class LiskovResolutionTest {
  implicitly[Liskov[Nothing, Any, Int, AnyVal]]
  implicitly[Liskov[Int, Any, Int, AnyVal]]
  implicitly[Liskov[Int, AnyVal, Int, AnyVal]]
}
