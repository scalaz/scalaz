package scalaz.example.math

import scalaz._
import Scalaz._

object ExampleFAD {
  def main(args: Array[String]) = run

  def run {
    import scalaz.math._
    import FAD._

    def f[T](x: T)(implicit r: Real[T]) = {
      import r._

      x * 2. + x * x
    }

    // Evaluate the function at x = 1
    f(1.).toDouble assert_≟ 3.0

    // Calculate the differential of f with respect to x, at x = 1.
    diff[Double](f(_))(1.).toDouble assert_≟ 4.0
  }
}
