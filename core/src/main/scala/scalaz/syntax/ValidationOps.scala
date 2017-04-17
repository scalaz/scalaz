package scalaz
package syntax

final class ValidationOps[A](val self: A) extends AnyVal {
  def success[X]: Validation[X, A] = Validation.success[X, A](self)

  def successNel[X]: ValidationNel[X, A] = success

  def failure[X]: Validation[A, X] = Validation.failure[A, X](self)

  def failureNel[X]: ValidationNel[A, X] = Validation.failureNel[A, X](self)
}

trait ToValidationOps {
  implicit def ToValidationOps[A](a: A): ValidationOps[A] = new ValidationOps(a)
}
