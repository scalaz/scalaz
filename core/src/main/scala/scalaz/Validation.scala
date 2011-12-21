package scalaz

// TODO Variance removed since Scalaz6. Happy with that?
/**
 * Represents either:
 *  - `Success(a)`, or
 *  - `Failure(e)`.
 *
 * Isomorphic to `scala.Either`. The motivation for a `Validation` is to provide the instance
 * `Applicative[[a]Validation[E, a]]` that accumulate failures through a [[scalaz.Semigroup]]`[E]`.
 *
 * [[scalaz.NonEmptyList]] is commonly chosen as a type constructor for the type `E`. As a convenience,
 * an alias `scalaz.ValidationNEL[E]` is provided as a shorthand for `scalaz.Validation[NonEmptyList[E]]`,
 * and a method `Validation#toValidationNel` converts `Validation[E]` to `ValidationNEL[E]`.
 *
 * Example:
 * {{{
 * import scalaz._, std.AllInstances._
 *
 * def parseInt(s: String): Validation[String, Int] =
 *   try { Success(s.toInt) } catch { case ex: NumberFormatException => Failure(ex.getMessage) }
 * val V = Validation.validationNelApplicative[String]
 *
 * val x: ValidationNEL[String, Int] =
 *   V.map2(parseInt("1.x").toValidationNel, parseInt("1..0").toValidationNel)(_ * _)
 *   // Failure(NonEmptyList(For input string: "1..0", For input string: "1.x"))
 * }}}
 *
 * @tparam E The type of the `Failure`
 * @tparam A The type of the `Success`
 */
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

  /** Convert to a `scala.Either`. `Success` is converted to `scala.Right`, and `Failure` to `scala.Left`. */
  def either: Either[E, A] = this match {
    case Success(a) => Right(a)
    case Failure(e) => Left(e)
  }

  def isSuccess: Boolean = this match {
    case Success(_) => true
    case Failure(_) => false
  }

  def isFailure: Boolean = !isSuccess

  /** Returns the contents of this validation, in an `Some`, if it is a `Success`, otherwise `None` */
  def toOption: Option[A] = this match {
    case Success(a) => Some(a)
    case Failure(_) => None
  }

  /** If `this` and `that` are both `Success`, or both a `Failure`, combine them with the provided `Semigroup. Otherwise, return the `Success`. */
  def append(that: Validation[E, A])(implicit es: Semigroup[E], as: Semigroup[A]): Validation[E, A] = (this, that) match {
    case (Success(a1), Success(a2))   => Success(as.append(a1, a2))
    case (v1@Success(a1), Failure(_)) => v1
    case (Failure(_), v2@Success(a2)) => v2
    case (Failure(e1), Failure(e2))   => Failure(es.append(e1, e2))
  }

  /** An alias for `append` */
  def >>*<<(x: Validation[E, A])(implicit es: Semigroup[E], as: Semigroup[A]): Validation[E, A] = append(x)

  /** A view of this validation from the `Failure` side. */
  def fail: FailProjection[E, A] = new FailProjection[E, A] {
    val validation = Validation.this
  }

  /** Wrap the success value in `M` */
  def point[M[_] : Pointed]: Validation[E, M[A]] = this match {
    case Success(a) => Success(Pointed[M].point(a: A))
    case Failure(e) => Failure(e)
  }

  /** Wraps the failure value in a [[scalaz.NonEmptyList]] */
  def toValidationNel: ValidationNEL[E, A] = fail.point[NonEmptyList]

  /**
   * Returns the contents of this `Validation` if it is a `Success`, otherwise applies to the contents of the `Failure`
   * to the provided function
   */
  def |||(f: E => A): A = this match {
    case Success(a) => a
    case Failure(e) => f(e)
  }

  /**
   * Returns `this` if it is a `Success`, or `that` if it is a `Success`, otherwise a `Failure` containing
   * the contents of the two `Failure`s, appended with the provided `Semigroup`.
   */
  def orElse(that: => Validation[E, A])(implicit E: Semigroup[E]): Validation[E, A] = this match {
    case v1@Success(a1) => v1
    case Failure(e1)    => that match {
      case v2@Success(a2) => v2
      case Failure(e2)    => Failure(E.append(e1, e2))
    }
  }

  /** Returns the contents of this `Validation` it if is a `Success`, otherwise the provided value. */
  def getOrElse(f: => A): A = |||(_ => f)

  /** An alias for `getOrElse` */
  def |(f: => A): A = getOrElse(f)

  def exists(f: A => Boolean): Boolean = this match {
    case Success(a) => f(a)
    case Failure(_) => false
  }

  def forall(f: A => Boolean): Boolean = this match {
    case Success(a) => f(a)
    case Failure(_) => true
  }

  def traverse[G[_] : Applicative, B](f: A => G[B]): G[Validation[E, B]] = this match {
    case Success(a) => Applicative[G].map(f(a))(Success(_))
    case Failure(e) => Applicative[G].point(Failure(e))
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Success(a) => f(a, z)
    case Failure(e) => z
  }

  def ap[B](f: => Validation[E, A => B])(implicit E: Semigroup[E]): Validation[E, B] = (this, f) match {
    case (Success(a), Success(f))   => Success(f(a))
    case (Failure(e), Success(_))   => Failure(e)
    case (Success(f), Failure(e))   => Failure(e)
    case (Failure(e1), Failure(e2)) => Failure(E.append(e1, e2))
  }

  def bimap[C, D](f: (E) => C, g: (A) => D): Validation[C, D] = this match {
    case Failure(a) => Failure(f(a))
    case Success(b) => Success(g(b))
  }

  def bitraverse[G[_] : Applicative, C, D](f: (E) => G[C], g: (A) => G[D]): G[Validation[C, D]] = this match {
    case Failure(a) => Applicative[G].map(f(a))(Failure(_))
    case Success(b) => Applicative[G].map(g(b))(Success(_))
  }
}

// TODO private?
final case class Success[E, A](a: A) extends Validation[E, A]

final case class Failure[E, A](e: E) extends Validation[E, A]

sealed trait FailProjection[E, A] {

  import Validation._

  val validation: Validation[E, A]

  def toOption: Option[E] = validation match {
    case Success(_) => None
    case Failure(e) => Some(e)
  }

  /** Wrap the failure value in `M` */
  def point[M[_] : Pointed]: Validation[M[E], A] = validation match {
    case Success(a) => Success(a)
    case Failure(e) => Failure(Pointed[M].point(e: E))
  }

  /** Returns the contents of this `Validation` if it is a `Failure`, otherwise applies contents of the 'Success' to the provided function */
  def |||(f: A => E): E = validation match {
    case Success(a) => f(a)
    case Failure(e) => e
  }

  /** Returns the contents of this `Validation` if it is a `Failure`, otherwise the provided value */
  def getOrElse(f: => E): E = |||(_ => f)

  /** An alias for `getOrElse` */
  def |(f: => E): E = getOrElse(f)

  def exists(f: E => Boolean): Boolean = validation match {
    case Success(_) => false
    case Failure(e) => f(e)
  }

  def forall(f: E => Boolean): Boolean = validation match {
    case Success(_) => true
    case Failure(e) => f(e)
  }
}

object FailProjection extends FailProjectionFunctions with FailProjectionInstances {
  def apply[A]: (Id ~> ({type λ[α] = Validation[α, A]})#λ) =
    Validation.failureNT[A]
}

trait FailProjectionInstances {
  import FailProjection._

  /**Derive the type class instance for `FailProjection` from `Validation`. */
  implicit def failProjectionApplicative[E](implicit E: Semigroup[E]) = {
    type F[a] = FailProjection[E, a]
    type G[a] = Validation[E, a]

    new IsomorphismTraverse[F, G] with IsomorphismApplicative[F, G] {
      def iso = FailProjectionEIso2[E]
      implicit def G = Validation.validationApplicative(E)
    }
  }

  /** Intentionally non-implicit */
  def failProjectionMonad[E] = {
    type F[a] = FailProjection[E, a]
    type G[a] = Validation[E, a]

    new IsomorphismPointed[F, G] with IsomorphismTraverse[F, G] with IsomorphismMonad[F, G] {
      def iso = FailProjectionEIso2[E]
      implicit def G = Validation.validationMonad
    }
  }

  implicit def failProjectionOrder[E: Order, X: Order] = new IsomorphismOrder[FailProjection[E, X], Validation[E, X]] {
    def iso = FailProjectionIso
    implicit def G = Validation.validationOrder
  }
}

trait FailProjectionFunctions {
  import Isomorphism._

  /** FailProjection is isomorphic to Validation */
  implicit def FailProjectionIso[E, A] = new (FailProjection[E, A] <=> Validation[E, A]) {
    def to: (FailProjection[E, A]) => Validation[E, A] = _.validation
    def from: (Validation[E, A]) => FailProjection[E, A] = _.fail
  }

  /** FailProjection is isomorphic to Validation, when the type parameter `E` is partially applied. */
  implicit def FailProjectionEIso2[E] = new IsoFunctorTemplate[({type λ[α]=FailProjection[E, α]})#λ, ({type λ[α]=Validation[E, α]})#λ] {
    def to[A](fa: FailProjection[E, A]) = fa.validation
    def from[A](ga: Validation[E, A]) = ga.fail
  }

  /** FailProjection is isomorphic to Validation, when the type parameter `A` is partially applied. */
  implicit def FailProjectionAIso2[A] = new IsoFunctorTemplate[({type λ[α]=FailProjection[α, A]})#λ, ({type λ[α]=Validation[α, A]})#λ] {
    def to[E](fa: FailProjection[E, A]) = fa.validation
    def from[E](ga: Validation[E, A]) = ga.fail
  }
}

object Validation extends ValidationFunctions with ValidationInstances

trait ValidationInstances {
  /**Validation is an Applicative Functor, if the error type forms a Semigroup */
  implicit def validationApplicative[E](implicit E: Semigroup[E]) = new Traverse[({type λ[α] = Validation[E, α]})#λ]
    with Applicative[({type λ[α] = Validation[E, α]})#λ] with Plus[({type λ[α] = Validation[E, α]})#λ] {
    def point[A](a: => A): Validation[E, A] = Success(a)

    def traverseImpl[G[_] : Applicative, A, B](fa: Validation[E, A])(f: A => G[B]): G[Validation[E, B]] = fa traverse f

    def foldRight[A, B](fa: Validation[E, A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)

    def ap[A, B](fa: => Validation[E, A])(f: => Validation[E, A => B]): Validation[E, B] = fa ap f

    def plus[A](a: Validation[E, A], b: => Validation[E, A]): Validation[E, A] = a orElse b
  }

  def validationNelApplicative[E] = validationApplicative[NonEmptyList[E]]

  implicit def validationBiTraverse = new BiTraverse[Validation] {
    override def bimap[A, B, C, D](fab: Validation[A, B])(f: A => C, g: B => D): Validation[C, D] = fab.bimap(f, g)

    def bitraverse[G[_] : Applicative, A, B, C, D](fab: Validation[A, B])(f: (A) => G[C], g: (B) => G[D]) = fab.bitraverse[G, C, D](f, g)
  }

  // Intentionally non-implicit to avoid accidentally using this where Applicative is preferred
  def validationMonad[E] = new Traverse[({type λ[α] = Validation[E, α]})#λ] with Monad[({type λ[α] = Validation[E, α]})#λ] {
    def point[A](a: => A): Validation[E, A] = Success(a)

    def traverseImpl[G[_] : Applicative, A, B](fa: Validation[E, A])(f: A => G[B]): G[Validation[E, B]] = fa traverse f

    def foldRight[A, B](fa: Validation[E, A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)

    def bind[A, B](fa: Validation[E, A])(f: A => Validation[E, B]): Validation[E, B] = fa flatMap f
  }

  implicit def validationOrder[E: Order, A: Order]: Order[Validation[E, A]] = new Order[Validation[E, A]] {
    import Ordering._
    def order(f1: Validation[E, A], f2: Validation[E, A]) = (f1, f2) match {
      case (Success(x), Success(y)) => Order[A].order(x, y)
      case (Failure(x), Failure(y)) => Order[E].order(x, y)
      case (Failure(_), Success(_)) => LT
      case (Success(_), Failure(_)) => GT
    }
  }
}

trait ValidationFunctions {
  def success[E, A](a: A): Validation[E, A] = Success(a)

  def failure[E, A](e: E): Validation[E, A] = Failure(e)

  def fromEither[E, A](e: Either[E, A]): Validation[E, A] =
    e.fold(e => Failure(e), a => Success(a))

  def fromTryCatch[T](a: => T): Validation[Throwable, T] = try {
    success(a)
  } catch {
    case e => failure(e)
  }

  def successNT[E]: (Id ~> ({type λ[α] = Validation[E, α]})#λ) =
    new (Id ~> ({type λ[α] = Validation[E, α]})#λ) {
      def apply[A](a: A) = Success(a)
    }

  def failureNT[A]: (Id ~> ({type λ[α] = Validation[α, A]})#λ) =
    new (Id ~> ({type λ[α] = Validation[α, A]})#λ) {
      def apply[E](e: E) = Failure(e)
    }
}
