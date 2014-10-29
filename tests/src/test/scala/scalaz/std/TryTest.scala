package scalaz
package std

import std.AllInstances._
import org.scalacheck.Prop.forAll
import scala.util.{Failure, Success, Try}

object TryTest extends SpecLite {
  "toDisjunction Failure" ! forAll { t: Throwable =>
    `try`.toDisjunction(Failure[Int](t)).isLeft
  }

  "toDisjunction Success" ! forAll { i: Int =>
    `try`.toDisjunction(Success(i)).isRight
  }
}
