package scalaz

sealed trait Validation[+E, +A] {
  import S._
  
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

  def fail = new FailProjection

  ////

  def lift[M[+_]](implicit p: Pure[M]): Validation[E, M[A]] = this match {
    case Success(a) => Success(a.pure[M])
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

  final class FailProjection {
    def lift[M[+_]](implicit p: Pure[M]): Validation[M[E], A] = Validation.this match {
      case Success(a) => Success(a)
      case Failure(e) => Failure(e.pure[M])
    }

    def |||[EE >: E](f: A => EE): EE = Validation.this match {
      case Success(a) => f(a)
      case Failure(e) => e
    }

    def |[EE >: E](f: => EE) = |||[EE](_ => f)

    def exists(f: E => Boolean) = Validation.this match {
      case Success(_) => false
      case Failure(e) => f(e)
    }

    def forall(f: E => Boolean) = Validation.this match {
      case Success(_) => true
      case Failure(e) => f(e)
    }
  }
}

final case class Success[E, A](a: A) extends Validation[E, A]
final case class Failure[E, A](e: E) extends Validation[E, A]
