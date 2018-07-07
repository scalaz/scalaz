package scalaz

import scala.{ Any, AnyVal, Nothing }

import Predef._
import prop.Leibniz

class LeibnizResolutionTest {
  implicitly[Leibniz[Nothing, Any, Int, Int]]
  implicitly[Leibniz[Int, Any, Int, Int]]
  implicitly[Leibniz[Int, AnyVal, Int, Int]]
  implicitly[Leibniz[Nothing, Any, AnyVal, AnyVal]]
  implicitly[Leibniz[Int, Any, AnyVal, AnyVal]]
  implicitly[Leibniz[Int, AnyVal, AnyVal, AnyVal]]
  implicitly[Leibniz[Nothing, Nothing, Nothing, Nothing]]
}
