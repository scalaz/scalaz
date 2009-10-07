package scalaz

sealed trait Validation[+E, +A] {
  import Scalaz._
  
  def either = this match {
    case Success(a) => Right(a)
    case Failure(e) => Left(e)
  }

  def isSuccess = this match {
    case Success(_) => true
    case Failure(_) => false
  }

  def isFailure = !isSuccess

  def success = this match {
    case Success(a) => Some(a)
    case Failure(_) => None
  }

  def failure = this match {
    case Success(_) => None
    case Failure(e) => Some(e)
  }

  def >>*<<[EE >: E, AA >: A](x: Validation[EE, AA])(implicit m: Semigroup[AA], s: Semigroup[EE]): Validation[EE, AA] = (this, x) match {
    case (Success(a1), Success(a2)) => Success((a1: AA) |+| a2)
    case (Success(a1), Failure(_)) => Success(a1)
    case (Failure(_), Success(a2)) => Success(a2)
    case (Failure(e1), Failure(e2)) => Failure((e1: EE) |+| e2)
  }

  def fail = new Validation.FailureProjection[E, A](this)

  ////

  def lift[M[_], AA >: A](implicit p: Pure[M]): Validation[E, M[AA]] = this match {
    case Success(a) => Success((a: AA).pure[M])
    case Failure(e) => Failure(e)
  }

  def |||[AA >: A](f: E => AA): AA = this match {
    case Success(a) => a
    case Failure(e) => f(e)
  }

  def |[AA >: A](f: => AA) = |||[AA](_ => f)

  def exists(f: A => Boolean) = this match {
    case Success(a) => f(a)
    case Failure(_) => false
  }

  def forall(f: A => Boolean) = this match {
    case Success(a) => f(a)
    case Failure(_) => true
  }
}

final case class Success[E, A](a: A) extends Validation[E, A]
final case class Failure[E, A](e: E) extends Validation[E, A]

object Validation {
  import Scalaz._

  final case class FailureProjection[+E, +A](validation: Validation[E, A]) {
    def lift[M[_], EE >: E](implicit p: Pure[M]): Validation[M[EE], A] = validation match {
      case Success(a) => Success(a)
      case Failure(e) => Failure((e: EE).pure[M])
    }

    def |||[EE >: E](f: A => EE): EE = validation match {
      case Success(a) => f(a)
      case Failure(e) => e
    }

    def |[EE >: E](f: => EE) = |||[EE](_ => f)

    def exists(f: E => Boolean) = validation match {
      case Success(_) => false
      case Failure(e) => f(e)
    }

    def forall(f: E => Boolean) = validation match {
      case Success(_) => true
      case Failure(e) => f(e)
    }
  }
}
