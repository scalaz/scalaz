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
}

trait TryInstances {
  import scalaz.std.{`try` => t}

  val tryDisjunctionIso: Try <~> λ[α => Throwable \/ α] =
    new IsoFunctorTemplate[Try, Throwable \/ ?] {
      def to[A](fa: Try[A]) = t.toDisjunction(fa)
      def from[A](ga: Throwable \/ A) = t.fromDisjunction(ga)
    }
}

object `try` extends TryFunctions with TryInstances
