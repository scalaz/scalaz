package scalaz
package std

import scala.util.{Failure, Success, Try}

trait TryFunctions {
  def cata[A, B](t: Try[A])(success: A => B, failure: Throwable => B): B =
    t match {
      case Success(a) => success(a)
      case Failure(t) => failure(t)
    }

  def toDisjunction[A](t: Try[A]): Throwable \/ A =
    cata(t)(\/.right, \/.left)
}

object `try` extends TryFunctions
