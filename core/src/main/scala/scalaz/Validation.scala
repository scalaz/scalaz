package scalaz

sealed trait Validation[+E, +A] {
  import Scalaz._

  def fold[X](failure: E => X = identity[E] _, success: A => X = identity[A] _): X = this match {
    case Success(x) => success(x)
    case Failure(x) => failure(x)
  }

  def either : Either[E, A] = this match {
    case Success(a) => Right(a)
    case Failure(e) => Left(e)
  }

  def isSuccess : Boolean = this match {
    case Success(_) => true
    case Failure(_) => false
  }

  def isFailure : Boolean = !isSuccess

  def toOption : Option[A] = this match {
    case Success(a) => Some(a)
    case Failure(_) => None
  }

  def >>*<<[EE >: E: Semigroup, AA >: A: Semigroup](x: Validation[EE, AA]): Validation[EE, AA] = (this, x) match {
    case (Success(a1), Success(a2)) => Success((a1: AA) ⊹ a2)
    case (Success(a1), Failure(_)) => Success(a1)
    case (Failure(_), Success(a2)) => Success(a2)
    case (Failure(e1), Failure(e2)) => Failure((e1: EE) ⊹ e2)
  }

  def fail : FailProjection[E, A] = new FailProjection[E, A] {
    val validation = Validation.this
  }

  def lift[M[_]: Pure, AA >: A]: Validation[E, M[AA]] = this match {
    case Success(a) => Success((a: AA) η)
    case Failure(e) => Failure(e)
  }

  /**
   * Wraps the failure value in a NonEmptyList
   */
  def liftFailNel: Validation[NonEmptyList[E], A] = fail.liftNel

  def |||[AA >: A](f: E => AA): AA = this match {
    case Success(a) => a
    case Failure(e) => f(e)
  }

  def |[AA >: A](f: => AA): AA = |||[AA](_ => f)

  def exists(f: A => Boolean): Boolean = this match {
    case Success(a) => f(a)
    case Failure(_) => false
  }

  def forall(f: A => Boolean): Boolean = this match {
    case Success(a) => f(a)
    case Failure(_) => true
  }
}

final case class Success[E, A](a: A) extends Validation[E, A]
final case class Failure[E, A](e: E) extends Validation[E, A]

sealed trait FailProjection[+E, +A] {
  val validation: Validation[E, A]

  import Scalaz._

  def toOption: Option[E] = validation match {
    case Success(_) => None
    case Failure(e) => Some(e)
  }
  
  def lift[M[_]: Pure, EE >: E]: Validation[M[EE], A] = validation match {
    case Success(a) => Success(a)
    case Failure(e) => Failure((e: EE) η)
  }

  def liftNel: Validation[NonEmptyList[E], A] = lift[NonEmptyList, E]

  def |||[EE >: E](f: A => EE): EE = validation match {
    case Success(a) => f(a)
    case Failure(e) => e
  }

  def |[EE >: E](f: => EE): EE = |||[EE](_ => f)

  def exists(f: E => Boolean): Boolean = validation match {
    case Success(_) => false
    case Failure(e) => f(e)
  }

  def forall(f: E => Boolean): Boolean = validation match {
    case Success(_) => true
    case Failure(e) => f(e)
  }
}

trait Validations {
  type ValidationNEL[E, X] = Validation[NonEmptyList[E], X]

  def success[E, A](a: A): Validation[E, A] = Success(a)

  def failure[E, A](e: E): Validation[E, A] = Failure(e)

  def validation[E, A](e: Either[E, A]): Validation[E, A] = e.fold(Failure(_), Success(_))
}
