package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll
import scala.util.{Failure, Success}

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

  "toValidation Failure" ! forAll { t: Throwable =>
    `try`.toValidation(Failure[Int](t)).isFailure
  }

  "toValidation Success" ! forAll { i: Int =>
    `try`.toValidation(Success[Int](i)).isSuccess
  }

  "toValidationNel Failure" ! forAll {t: Throwable =>
    `try`.toValidationNel(Failure[Int](t)).isFailure
  }

  "toValidationNel Success" ! forAll {i: Int =>
    `try`.toValidationNel(Success[Int](i)).isSuccess
  }

  "validation round trip" ! forAll { v: Validation[Throwable, Int] =>
    val iso = `try`.tryValidationIso
    iso.to(iso.from(v)) must_=== v
   }
}
