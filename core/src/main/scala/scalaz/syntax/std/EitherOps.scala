package scalaz
package syntax
package std

final class EitherOps[A, B](private val self: Either[A, B]) extends AnyVal {

  final def toDisjunction: A \/ B = \/ fromEither self

  final def toValidation: Validation[A, B] = Validation fromEither self
}

final class RightOps[A, B](private val self: Right[A, B]) extends AnyVal {

  def coerceLeft[C]: Either[C, B] = self.asInstanceOf[Either[C, B]]

}

final class LeftOps[A, B](private val self: Left[A, B]) extends AnyVal {

  def coerceRight[C]: Either[A, C] = self.asInstanceOf[Either[A, C]]

}

trait ToEitherOps {
  implicit def ToEitherOpsFromEither[A, B](e: Either[A, B]): EitherOps[A, B] = new EitherOps(e)
  implicit def ToRightOpsFromRight[A, B](e: Right[A, B]): RightOps[A, B] = new RightOps(e)
  implicit def ToLeftOpsFromLeft[A, B](e: Left[A, B]): LeftOps[A, B] = new LeftOps(e)
}

