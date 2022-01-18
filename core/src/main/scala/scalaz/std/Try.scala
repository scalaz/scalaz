package scalaz
package std

import Isomorphism.{<~>, IsoFunctorTemplate}
import scala.util.{Failure, Success, Try}

trait TryFunctions {
  def cata[A, B](t: Try[A])(success: A => B, failure: Throwable => B): B =
    t match {
      case Success(a) => success(a)
      case Failure(t) => failure(t)
    }

  def toDisjunction[A](t: Try[A]): Throwable \/ A =
    cata(t)(\/.right, \/.left)

  def fromDisjunction[T <: Throwable, A](d: T \/ A): Try[A] =
    d.fold(Failure.apply, Success.apply)

  def toValidation[A](t: Try[A]): Validation[Throwable, A] =
    cata(t)(Validation.success, Validation.failure)

  def toValidationNel[A](t: Try[A]) : ValidationNel[Throwable, A] =
    cata(t)(Validation.success, Validation.failureNel)

  def fromValidation[T <: Throwable, A](v: Validation[T, A]) : Try[A] =
    v.fold(Failure.apply, Success.apply)
}

trait TryInstances {
  import scalaz.std.{`try` => t}

  val tryDisjunctionIso: Try <~> λ[α => Throwable \/ α] =
    new IsoFunctorTemplate[Try, \/[Throwable, *]] {
      def to[A](fa: Try[A]) = t.toDisjunction(fa)
      def from[A](ga: Throwable \/ A) = t.fromDisjunction(ga)
    }

  val tryValidationIso: Try <~> λ[α => Validation[Throwable, α]] =
    new IsoFunctorTemplate[Try, Validation[Throwable, *]] {
      def to[A](fa: Try[A]) = t.toValidation(fa)
      def from[A](v: Validation[Throwable, A]) = t.fromValidation(v)
    }
}

object `try` extends TryFunctions with TryInstances
