package scalaz
package syntax

trait ValidationV[A] extends Ops[A] {
  def success[X]: Validation[X, A] = Validation.success[X, A](self)

  def successNel[X]: ValidationNel[X, A] = success

  def failure[X]: Validation[A, X] = Validation.failure[A, X](self)

  def fail[X]: Validation[A, X] = failure[X]

  def failureNel[X]: ValidationNel[A, X] = Validation.failure[NonEmptyList[A], X](NonEmptyList(self))

  def failNel[X]: ValidationNel[A, X] = failureNel[X]
}

trait ToValidationOps {
  implicit def ToValidationV[A](a: A) = new ValidationV[A]{ def self = a }
}
