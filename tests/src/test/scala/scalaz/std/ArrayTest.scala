package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._

object ArrayTest extends SpecLite {
  "show" in {
    import syntax.show._

    val arr: Array[Int] = Array(1, 2, 3, 4)

    arr.shows must_===("[1,2,3,4]")
  }
}
