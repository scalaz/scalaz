package scalaz
package syntax
package std

final class EitherOps[A, B](private val self: Either[A, B]) extends AnyVal {

  final def toDisjunction: A \/ B = \/ fromEither self

  @deprecated("Use `toDisjunction`", "7.3.0")
  final def disjunction: A \/ B = toDisjunction

  final def toValidation: Validation[A, B] = Validation fromEither self

  @deprecated("Use `toValidation`", "7.3.0")
  final def validation: Validation[A, B] = toValidation
}

trait ToEitherOps {
  implicit def ToEitherOpsFromEither[A, B](e: Either[A, B]): EitherOps[A, B] = new EitherOps(e)
}

