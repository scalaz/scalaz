import scalaz.data.Liskov

class LiskovResolutionTest {
  implicitly[Liskov[Nothing, Any, Int, AnyVal]]
  implicitly[Liskov[Int, Any, Int, AnyVal]]
  implicitly[Liskov[Int, AnyVal, Int, AnyVal]]
}
