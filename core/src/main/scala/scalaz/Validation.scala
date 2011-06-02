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

object FailProjection {
  implicit def FailProjection_^*^[E, A]: (FailProjection[E, A] ^*^ Validation[E, A]) =
    ^*^.^*^(_.validation, b => new FailProjection[E, A] {
      val validation = b
    })


  implicit def FailProjection_^**^[E] : (({type λ[α] = FailProjection[E, α]})#λ ^**^ ({type λ[α] = Validation[E, α]})#λ) =
    new (({type λ[α] = FailProjection[E, α]})#λ ^**^ ({type λ[α] = Validation[E, α]})#λ) {
      def unpack[A] = _.validation

      def pack[A] = b => new FailProjection[E, A] {
        val validation = b
      }
    }

  implicit def FailProjectionShow[E: Show, A: Show]: Show[FailProjection[E, A]] =
    Show.showBy(_.validation)

  implicit def FailProjectionEqual[E: Equal, A: Equal]: Equal[FailProjection[E, A]] =
    Equal.equalBy(_.validation)

  implicit def FailProjectionOrder[E: Order, A: Order]: Order[FailProjection[E, A]] =
    Order.orderBy(_.validation)

  implicit def FailProjectionFunctor[X]: Functor[({type λ[α] = FailProjection[X, α]})#λ] =
    implicitly[Functor[({type λ[α] = Validation[X, α]})#λ]].deriving[({type λ[α] = FailProjection[X, α]})#λ]

  implicit def FailProjectionPointed[X]: Pointed[({type λ[α] = FailProjection[X, α]})#λ] = 
    implicitly[Pointed[({type λ[α] = Validation[X, α]})#λ]].deriving[({type λ[α] = FailProjection[X, α]})#λ]

  implicit def FailProjectionPointedFunctor[X]: PointedFunctor[({type λ[α] = FailProjection[X, α]})#λ] =
    implicitly[PointedFunctor[({type λ[α] = Validation[X, α]})#λ]].deriving[({type λ[α] = FailProjection[X, α]})#λ]

  implicit def FailProjectionApplic[X: Semigroup]: Applic[({type λ[α] = FailProjection[X, α]})#λ] =
    implicitly[Applic[({type λ[α] = Validation[X, α]})#λ]].deriving[({type λ[α] = FailProjection[X, α]})#λ]

  implicit def FailProjectionApplicFunctor[X: Semigroup]: ApplicFunctor[({type λ[α] = FailProjection[X, α]})#λ] =
    implicitly[ApplicFunctor[({type λ[α] = Validation[X, α]})#λ]].deriving[({type λ[α] = FailProjection[X, α]})#λ]

  implicit def FailProjectionApplicative[X: Semigroup]: Applicative[({type λ[α] = FailProjection[X, α]})#λ] =
    implicitly[Applicative[({type λ[α] = Validation[X, α]})#λ]].deriving[({type λ[α] = FailProjection[X, α]})#λ]

}

import ~>._

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

  implicit def ValidationShow[E: Show, A: Show]: Show[Validation[E, A]] =
    Show.shows(_.fold(
      "Success(" + implicitly[Show[E]].shows(_) + ")"
      , "Failure(" + implicitly[Show[A]].shows(_) + ")"
    ))

  implicit def ValidationEqual[E: Equal, A: Equal]: Equal[Validation[E, A]] =
    Equal.equalBy(_.either)

  implicit def ValidationOrder[E: Order, A: Order]: Order[Validation[E, A]] =
    Order.orderBy(_.either)

  implicit def ValidationFunctor[X]: Functor[({type λ[α] = Validation[X, α]})#λ] = new Functor[({type λ[α] = Validation[X, α]})#λ] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

  implicit def ValidationPointed[X]: Pointed[({type λ[α] = Validation[X, α]})#λ] = new Pointed[({type λ[α] = Validation[X, α]})#λ] {
    def point[A](a: => A) =
      Success(a)
  }

  implicit def ValidationPointedFunctor[X]: PointedFunctor[({type λ[α] = Validation[X, α]})#λ] =
    PointedFunctor.pointedFunctor[({type λ[α] = Validation[X, α]})#λ]

  implicit def ValidationApplic[X: Semigroup]: Applic[({type λ[α] = Validation[X, α]})#λ] = new Applic[({type λ[α] = Validation[X, α]})#λ] {
    def applic[A, B](f: Validation[X, A => B]) =
      a => (f, a) match {
        case (Success(f), Success(a)) => success(f(a))
        case (Success(_), Failure(e)) => failure(e)
        case (Failure(e), Success(_)) => failure(e)
        case (Failure(e1), Failure(e2)) => failure(implicitly[Semigroup[X]].append(e1, e2))
      }
  }

  implicit def ValidationApplicFunctor[X: Semigroup]: ApplicFunctor[({type λ[α] = Validation[X, α]})#λ] =
    ApplicFunctor.applicFunctor[({type λ[α] = Validation[X, α]})#λ]

  implicit def ValidationApplicative[X: Semigroup]: Applicative[({type λ[α] = Validation[X, α]})#λ] =
    Applicative.applicative[({type λ[α] = Validation[X, α]})#λ]

}
