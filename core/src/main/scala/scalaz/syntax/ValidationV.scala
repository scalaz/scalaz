package scalaz
package syntax

trait ValidationV[A] extends Ops[A] {
  def success[X]: Validation[X, A] = Validation.success[X, A](self)

  import Validation._

  def successNel[X]: ValidationNEL[X, A] = success

  def fail[X]: Validation[A, X] = failure[A, X](self)

  def failNel[X]: ValidationNEL[A, X] = failure[NonEmptyList[A], X](NonEmptyList(self))
}

trait ToValidationOps {
  implicit def ToValidationV[A](a: A) = new ValidationV[A]{ def self = a }
}
