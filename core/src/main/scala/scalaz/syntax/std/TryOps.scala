package scalaz
package syntax
package std

import scala.util.Try

import scalaz.std.{`try` => t}

final class TryOps[A](val self: Try[A]) extends AnyVal {

  final def cata[B](success: A => B, failure: Throwable => B): B =
    t.cata(self)(success, failure)

  final def toDisjunction: Throwable \/ A = t.toDisjunction(self)

  final def toValidation: Validation[Throwable, A] = t.toValidation(self)

  final def toValidationNel: ValidationNel[Throwable, A] = t.toValidationNel(self)
}

trait ToTryOps {
  implicit def ToTryOpsFromTry[A](a: Try[A]): TryOps[A] = new TryOps(a)
}
