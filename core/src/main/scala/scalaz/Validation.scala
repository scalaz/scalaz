package scalaz

sealed trait Validation[E, A] {

  import Validation._

  def fold[X](failure: E => X, success: A => X): X = this match {
    case Success(x) => success(x)
    case Failure(x) => failure(x)
  }

  def map[B](f: A => B): Validation[E, B] = this match {
    case Success(a) => Success(f(a))
    case Failure(e) => Failure(e)
  }

  def foreach[U](f: A => U): Unit = this match {
    case Success(a) => f(a)
    case Failure(e) =>
  }

  def flatMap[B](f: A => Validation[E, B]): Validation[E, B] = this match {
    case Success(a) => f(a)
    case Failure(e) => Failure(e)
  }

  def either: Either[E, A] = this match {
    case Success(a) => Right(a)
    case Failure(e) => Left(e)
  }

  def isSuccess: Boolean = this match {
    case Success(_) => true
    case Failure(_) => false
  }

  def isFailure: Boolean = !isSuccess

  def toOption: Option[A] = this match {
    case Success(a) => Some(a)
    case Failure(_) => None
  }

  def >>*<<(x: Validation[E, A])(implicit es: Semigroup[E], as: Semigroup[A]): Validation[E, A] = (this, x) match {
    case (Success(a1), Success(a2)) => Success(as.append(a1, a2))
    case (Success(a1), Failure(_)) => Success(a1)
    case (Failure(_), Success(a2)) => Success(a2)
    case (Failure(e1), Failure(e2)) => Failure(es.append(e1, e2))
  }

  def fail: FailProjection[E, A] = new FailProjection[E, A] {
    val validation = Validation.this
  }

  def pointSuccess[M[_] : Pointed]: Validation[E, M[A]] = this match {
    case Success(a) => Success(implicitly[Pointed[M]].point(a: A))
    case Failure(e) => Failure(e)
  }

  /**
   * Wraps the failure value in a NonEmptyList
   */
  def pointFailNel: ValidationNEL[E, A] = fail.pointFailNel

  def |||(f: E => A): A = this match {
    case Success(a) => a
    case Failure(e) => f(e)
  }

  def |(f: => A): A = |||(_ => f)

  def exists(f: A => Boolean): Boolean = this match {
    case Success(a) => f(a)
    case Failure(_) => false
  }

  def forall(f: A => Boolean): Boolean = this match {
    case Success(a) => f(a)
    case Failure(_) => true
  }
}

private final case class Success[E, A](a: A) extends Validation[E, A]

private final case class Failure[E, A](e: E) extends Validation[E, A]

sealed trait FailProjection[E, A] {

  import Validation._

  val validation: Validation[E, A]

  def toOption: Option[E] = validation match {
    case Success(_) => None
    case Failure(e) => Some(e)
  }

  def pointFail[M[_] : Pointed]: Validation[M[E], A] = validation match {
    case Success(a) => Success(a)
    case Failure(e) => Failure(implicitly[Pointed[M]].point(e: E))
  }

  def pointFailNel: ValidationNEL[E, A] = pointFail[NonEmptyList]

  def |||(f: A => E): E = validation match {
    case Success(a) => f(a)
    case Failure(e) => e
  }

  def |(f: => E): E = |||(_ => f)

  def exists(f: E => Boolean): Boolean = validation match {
    case Success(_) => false
    case Failure(e) => f(e)
  }

  def forall(f: E => Boolean): Boolean = validation match {
    case Success(_) => true
    case Failure(e) => f(e)
  }
}

import ~>._

object FailProjection extends FailProjections {
  def apply[A]: (I ~> ({type λ[α] = Validation[α, A]})#λ) =
    Validation.failure[A]
}

trait FailProjections {
  implicit def FailProjection_^*^[E, A]: (FailProjection[E, A] ^*^ Validation[E, A]) =
    ^*^.^*^(_.validation, b => new FailProjection[E, A] {
      val validation = b
    })


  implicit def FailProjection_^**^[E]: (({type λ[α] = FailProjection[E, α]})#λ ^**^ ({type λ[α] = Validation[E, α]})#λ) =
    new (({type λ[α] = FailProjection[E, α]})#λ ^**^ ({type λ[α] = Validation[E, α]})#λ) {
      def unpack[A] = _.validation

      def pack[A] = b => new FailProjection[E, A] {
        val validation = b
      }
    }
}

object Validation extends Validations {
  def apply[E]: (I ~> ({type λ[α] = Validation[E, α]})#λ) =
    success[E]
}

trait Validations {
  type ValidationNEL[E, X] = Validation[NonEmptyList[E], X]

  def success[E]: (I ~> ({type λ[α] = Validation[E, α]})#λ) =
    new (I ~> ({type λ[α] = Validation[E, α]})#λ) {
      def apply[A](a: A) = Success(a)
    }

  def failure[A]: (I ~> ({type λ[α] = Validation[α, A]})#λ) =
    new (I ~> ({type λ[α] = Validation[α, A]})#λ) {
      def apply[E](e: E) = Failure(e)
    }

  def fromEither[E, A](e: Either[E, A]): Validation[E, A] =
    e.fold(e => failure[A].apply[E](e), a => success[E].apply[A](a))

}
