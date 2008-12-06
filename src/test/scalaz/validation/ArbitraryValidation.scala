package scalaz.validation

import fjs.test.Arbitrary
import fjs.test.Arbitrary._

object ArbitraryValidation {
  implicit def ArbitraryValidation[A, B](implicit aa: Arbitrary[A], ab: Arbitrary[B]): Arbitrary[Validation[A, B]] =
    arbSEither(aa, ab) > (x => (x: Validation[A, B]))
}
