package scalaz
package syntax
package std

final class EitherOps[A, B](private val self: Either[A, B]) extends AnyVal {

  final def toDisjunction: A \/ B = \/ fromEither self

  final def toValidation: Validation[A, B] = Validation fromEither self
}

trait ToEitherOps {
  implicit def ToEitherOpsFromEither[A, B](e: Either[A, B]): EitherOps[A, B] = new EitherOps(e)
}

