package scalaz
package syntax

final class ValidationOps[A](self: A) {
  def success[X]: Validation[X, A] = Validation.success[X, A](self)

  def successNel[X]: ValidationNel[X, A] = success

  def failure[X]: Validation[A, X] = Validation.failure[A, X](self)

  @deprecated("use `failure` instead", "7.1")
  def fail[X]: Validation[A, X] = failure[X]

  def failureNel[X]: ValidationNel[A, X] = Validation.failureNel[A, X](self)

  @deprecated("use `failureNel` instead", "7.1")
  def failNel[X]: ValidationNel[A, X] = failureNel[X]
}

trait ToValidationOps {
  implicit def ToValidationOps[A](a: A) = new ValidationOps(a)
}
