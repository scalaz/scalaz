package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import scala.util.{Failure, Success, Try}

object TryTest extends SpecLite {

  // these probably aren't a great idea, but they are just for tests
  implicit val showThrowable: Show[Throwable] = Show.showA
  implicit val equalThrowable: Equal[Throwable] = Equal.equalA


  "toDisjunction Failure" ! forAll { t: Throwable =>
    `try`.toDisjunction(Failure[Int](t)).isLeft
  }

  "toDisjunction Success" ! forAll { i: Int =>
    `try`.toDisjunction(Success(i)).isRight
  }

  "disjunction round trip" ! forAll { d: Throwable \/ Int =>
    val iso = `try`.tryDisjunctionIso
    iso.to(iso.from(d)) must_=== d
  }
}
