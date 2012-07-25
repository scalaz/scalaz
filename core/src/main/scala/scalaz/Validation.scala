package scalaz

import Id._

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
sealed trait Validation[+E, +A] {

  import Validation._

  sealed trait SwitchingValidation[X] {
    def s: X
    def <<?:(fail: => X): X =
      Validation.this match {
        case Failure(_) => fail
        case Success(_) => s
      }
  }

  /** If this validation is success, return the given X value, otherwise, return the X value given to the return value. */
  def :?>>[X](success: => X): SwitchingValidation[X] =
    new SwitchingValidation[X] {
      def s = success
    }

  def isSuccess: Boolean = this match {
    case Success(_) => true
    case Failure(_) => false
  }

  def isFailure: Boolean = !isSuccess

  def fold[X](failure: E => X, success: A => X): X = this match {
    case Success(x) => success(x)
    case Failure(x) => failure(x)
  }

  /** Flip the faikure/success values in this validation. Alias for `swap` */
  def swap: Validation[A, E] =
    this match {
      case Failure(a) => Success(a)
      case Success(b) => Failure(b)
    }

  /** Flip the faikure/success values in this validation. Alias for `unary_~` */
  def unary_~ : Validation[A, E] =
    swap

  /** Run the given function on this swapped value. Alias for `~` */
  def swapped[EE >: E, AA >: A](k: Validation[A, E] => Validation[AA, EE]): Validation[EE, AA] =
    k(swap).swap

  /** Run the given function on this swapped value. Alias for `swapped` */
  def ~[EE >: E, AA >: A](k: Validation[A, E] => Validation[AA, EE]): Validation[EE, AA] =
    swapped(k)

  /** Binary functor map on this validation. */
  def bimap[C, D](f: E => C, g: A => D): Validation[C, D] =
    this match {
      case Failure(a) => Failure(f(a))
      case Success(b) => Success(g(b))
    }

  /** Binary functor traverse on this validation. */
  def bitraverse[G[+_] : Applicative, C, D](f: (E) => G[C], g: (A) => G[D]): G[Validation[C, D]] = this match {
    case Failure(a) => Applicative[G].map(f(a))(Failure(_))
    case Success(b) => Applicative[G].map(g(b))(Success(_))
  }

  /** Map on the success of this disjunction. */
  def map[B](f: A => B): Validation[E, B] = this match {
    case Success(a) => Success(f(a))
    case Failure(e) => Failure(e)
  }

  def traverse[G[+_] : Applicative, B](f: A => G[B]): G[Validation[E, B]] = this match {
    case Success(a) => Applicative[G].map(f(a))(Success(_))
    case Failure(e) => Applicative[G].point(Failure(e))
  }

  def foreach[U](f: A => U): Unit = this match {
    case Success(a) => f(a)
    case Failure(e) =>
  }

  /** Apply a function in the environment of the success of this validation, accumulating errors. */
  def ap[B, EE >: E](f: => Validation[EE, A => B])(implicit E: Semigroup[EE]): Validation[EE, B] = (this, f) match {
    case (Success(a), Success(f))   => Success(f(a))
    case (Failure(e), Success(_))   => Failure(e)
    case (Success(f), Failure(e))   => Failure(e)
    case (Failure(e1), Failure(e2)) => Failure(E.append(e2, e1))
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Success(a) => f(a, z)
    case Failure(e) => z
  }

  /** Filter on the success of this validation. */
  def filter[AA >: A](p: AA => Boolean)(implicit M: Monoid[AA]): Validation[E, AA] =
    this match {
      case Failure(a) => Failure(a)
      case Success(b) => Success(if(p(b)) b else M.zero)
    }

  def exists(f: A => Boolean): Boolean = this match {
    case Success(a) => f(a)
    case Failure(_) => false
  }

  def forall(f: A => Boolean): Boolean = this match {
    case Success(a) => f(a)
    case Failure(_) => true
  }

  /** Return an empty list or list with one element on the success of this validation. */
  def toList: List[A] =
    this match {
      case Failure(_) => Nil
      case Success(b) => List(b)
    }

  /** Return an empty stream or stream with one element on the success of this validation. */
  def toStream: Stream[A] =
    this match {
      case Failure(_) => Stream()
      case Success(b) => Stream(b)
    }

  /** Return an empty option or option with one element on the success of this validation. Useful to sweep errors under the carpet. */
  def toOption: Option[A] =
    this match {
      case Failure(_) => None
      case Success(b) => Some(b)
    }

  /** Convert to a core `scala.Either` at your own peril. */
  def toEither: Either[E, A] =
    this match {
      case Success(a) => Right(a)
      case Failure(e) => Left(e)
    }



  /** Convert to a `scalaz.\/`. `Success` is converted to `scalaz.\/-`, and `Failure` to `scalaz.-\/`. */
  def either: (E \/ A) = this match {
    case Success(a) => \/-(a)
    case Failure(e) => -\/(e)
  }

  /** If `this` and `that` are both `Success`, or both a `Failure`, combine them with the provided `Semigroup. Otherwise, return the `Success`. */
  def append[EE >: E, AA >: A](that: Validation[EE, AA])(implicit es: Semigroup[EE], as: Semigroup[AA]): Validation[EE, AA] = (this, that) match {
    case (Success(a1), Success(a2))   => Success(as.append(a1, a2))
    case (v1@Success(a1), Failure(_)) => v1
    case (Failure(_), v2@Success(a2)) => v2
    case (Failure(e1), Failure(e2))   => Failure(es.append(e1, e2))
  }

  /** An alias for `append` */
  def >>*<<[EE >: E, AA >: A](x: Validation[EE, AA])(implicit es: Semigroup[EE], as: Semigroup[AA]): Validation[EE, AA] = append(x)

  /** A view of this validation from the `Failure` side. */
  def fail: FailProjection[E, A] = new FailProjection[E, A] {
    val validation = Validation.this
  }

  /** Wrap the success value in `M`. */
  def pointSuccess[M[_] : Pointed, AA >: A]: Validation[E, M[AA]] = this match {
    case Success(a) => Success(Pointed[M].point(a: AA))
    case Failure(e) => Failure(e)
  }

  /** Wraps the failure value in a [[scalaz.NonEmptyList]] */
  def toValidationNel[EE >: E, AA >: A]: ValidationNEL[EE, AA] = fail.point[NonEmptyList, EE]

  /**
   * Returns the contents of this `Validation` if it is a `Success`, otherwise applies to the contents of the `Failure`
   * to the provided function
   */
  def |||[AA >: A](f: E => AA): AA = this match {
    case Success(a) => a
    case Failure(e) => f(e)
  }

  /**
   * Returns `this` if it is a `Success`, or `that` if it is a `Success`, otherwise a `Failure` containing
   * the contents of the two `Failure`s, appended with the provided `Semigroup`.
   */
  def orElse[EE >: E, AA >: A](that: => Validation[EE, AA])(implicit E: Semigroup[EE]): Validation[EE, AA] = this match {
    case v1@Success(a1) => v1
    case Failure(e1)    => that match {
      case v2@Success(a2) => v2
      case Failure(e2)    => Failure(E.append(e1, e2))
    }
  }

  /** Returns the contents of this `Validation` it if is a `Success`, otherwise the provided value. */
  def getOrElse[AA >: A](f: => AA): AA = |||(_ => f)

  /** An alias for `getOrElse` */
  def |[AA >: A](f: => AA): AA = getOrElse(f)

  def ensure[EE >: E](onFailure: => EE)(f: A => Boolean): Validation[EE, A] = this match {
    case succ @ Success(a) => if (f(a)) succ else Failure(onFailure)
    case failure => failure
  }


  /** Convert to a disjunction. */
  def disjunction: (E \/ A) =
    this match {
      case Success(a) => \/-(a)
      case Failure(e) => -\/(e)
    }

  /** Run a disjunction function and back to validation again. */
  def disjunctioned[EE >: E, AA >: A](k: (E \/ A) => (EE \/ AA)): Validation[EE, AA] =
    k(disjunction).validation

}

final case class Success[E, A](a: A) extends Validation[E, A]

final case class Failure[E, A](e: E) extends Validation[E, A]

sealed trait FailProjection[+E, +A] {

  import Validation._

  val validation: Validation[E, A]

  def toOption: Option[E] = validation match {
    case Success(_) => None
    case Failure(e) => Some(e)
  }

  /** Wrap the failure value in `M` */
  def point[M[_] : Pointed, EE >: E]: Validation[M[EE], A] = validation match {
    case Success(a) => Success(a)
    case Failure(e) => Failure(Pointed[M].point(e: EE))
  }

  /** Returns the contents of this `Validation` if it is a `Failure`, otherwise applies contents of the 'Success' to the provided function */
  def |||[EE >: E](f: A => EE): EE = validation match {
    case Success(a) => f(a)
    case Failure(e) => e
  }

  /** Returns the contents of this `Validation` if it is a `Failure`, otherwise the provided value */
  def getOrElse[EE >: E](f: => EE): EE = |||(_ => f)

  /** An alias for `getOrElse` */
  def |[EE >: E](f: => EE): EE = getOrElse(f)

  def exists(f: E => Boolean): Boolean = validation match {
    case Success(_) => false
    case Failure(e) => f(e)
  }

  def forall(f: E => Boolean): Boolean = validation match {
    case Success(_) => true
    case Failure(e) => f(e)
  }

  def map[B](f: E => B): FailProjection[B, A] = validation match {
    case Success(a) => Success[B, A](a).fail
    case Failure(e) => Failure(f(e)).fail
  }
}

object FailProjection extends FailProjectionFunctions with FailProjectionInstances {
  def apply[A]: (Id ~> ({type λ[α] = Validation[α, A]})#λ) =
    Validation.failureNT[A]
}

trait FailProjectionInstances0 {
  import FailProjection._

  implicit def failProjectionEqual[E: Equal, X: Equal] = new IsomorphismEqual[FailProjection[E, X], Validation[E, X]] {
    def iso = FailProjectionIso
    implicit def G = Validation.validationEqual
  }

  /**Derive the type class instance for `FailProjection` from `Validation`. */
  implicit def failProjectionPointed[E] = new IsomorphismPointed[({type λ[α] = FailProjection[E, α]})#λ, ({type λ[α] = Validation[E, α]})#λ] {
    def iso = FailProjectionEIso2[E]
    implicit def G = Validation.validationPointed[E]
  }
}

trait FailProjectionInstances extends FailProjectionInstances0 {
  import FailProjection._

  /**Derive the type class instance for `FailProjection` from `Validation`. */
  implicit def failProjectionApplicative[E](implicit E: Semigroup[E]) = {
    type F[a] = FailProjection[E, a]
    type G[a] = Validation[E, a]

    new IsomorphismTraverse[F, G] with IsomorphismApplicative[F, G] {
      def iso = FailProjectionEIso2[E]
      implicit def G = Validation.validationTraverseApplicativePlus(E)
    }
  }

  implicit def failProjectionSemigroup[E, A](implicit E0: Semigroup[E]): Semigroup[FailProjection[E, A]] = new IsomorphismSemigroup[FailProjection[E, A], Validation[E, A]] {
    def iso = FailProjectionIso
    implicit def G: Semigroup[Validation[E, A]] = Validation.validationSemigroup
  }

  implicit def failProjectionOrder[E: Order, X: Order] = new IsomorphismOrder[FailProjection[E, X], Validation[E, X]] {
    def iso = FailProjectionIso
    implicit def G = Validation.validationOrder
  }

  implicit def failProjectionShow[E: Show, X: Show] = new IsomorphismShow[FailProjection[E, X], Validation[E, X]] {
    def iso = FailProjectionIso
    implicit def G = Validation.validationShow
  }

  implicit def failProjectionApplicativeTraversePlus[E: Semigroup] =
    new IsomorphismApplicative[({type λ[α] = FailProjection[E, α]})#λ, ({type λ[α] = Validation[E, α]})#λ] with
      IsomorphismTraverse[({type λ[α] = FailProjection[E, α]})#λ, ({type λ[α] = Validation[E, α]})#λ] with
      IsomorphismPlus[({type λ[α] = FailProjection[E, α]})#λ, ({type λ[α] = Validation[E, α]})#λ] {
      def iso = FailProjectionEIso2
      implicit def G = Validation.validationTraverseApplicativePlus
    }

  implicit def failProjectionBitraverse =
    new IsomorphismBitraverse[FailProjection, Validation] {
      def iso = FailProjectionBiIso
      implicit def G = Validation.validationBitraverse
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

  /** The FailProjection type constructor is isomorphic to Validation */
  implicit def FailProjectionBiIso[A] = new IsoBifunctorTemplate[FailProjection, Validation] {
    def to[A, B](fa: FailProjection[A, B]): Validation[A, B] = fa.validation
    def from[A, B](ga: Validation[A, B]): FailProjection[A, B] = ga.fail
  }
}

object Validation extends ValidationFunctions with ValidationInstances

trait ValidationInstances0 {
  implicit def validationEqual[E: Equal, A: Equal]: Equal[Validation[E, A]] = new Equal[Validation[E, A]] {
    def equal(v1: Validation[E, A], v2: Validation[E, A]): Boolean = (v1, v2) match {
      case (Success(a1), Success(a2)) => Equal[A].equal(a1, a2)
      case (Failure(e1), Failure(e2)) => Equal[E].equal(e1, e2)
      case _                          => false
    }
  }

  implicit def validationPointed[E]: Pointed[({type λ[α] = Validation[E, α]})#λ] = new Pointed[({type λ[α] = Validation[E, α]})#λ] {
    def point[A](a: => A): Validation[E, A] = Success(a)

    def map[A, B](fa: Validation[E, A])(f: A => B): Validation[E, B] = fa map f
  }
}

trait ValidationInstances extends ValidationInstances0 {
  /**Validation is an Applicative Functor, if the error type forms a Semigroup */
  implicit def validationTraverseApplicativePlus[E](implicit E: Semigroup[E]) = new Traverse[({type λ[α] = Validation[E, α]})#λ]
    with Applicative[({type λ[α] = Validation[E, α]})#λ] with Plus[({type λ[α] = Validation[E, α]})#λ] {
    def point[A](a: => A): Validation[E, A] = Success(a)

    def traverseImpl[G[+_] : Applicative, A, B](fa: Validation[E, A])(f: A => G[B]): G[Validation[E, B]] = fa traverse f

    override def foldRight[A, B](fa: Validation[E, A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)

    def ap[A, B](fa: => Validation[E, A])(f: => Validation[E, A => B]): Validation[E, B] = fa ap f

    def plus[A](a: Validation[E, A], b: => Validation[E, A]): Validation[E, A] = a orElse b
  }

  def validationNelApplicative[E] = validationTraverseApplicativePlus[NonEmptyList[E]]

  implicit def validationBitraverse = new Bitraverse[Validation] {
    override def bimap[A, B, C, D](fab: Validation[A, B])(f: A => C, g: B => D): Validation[C, D] = fab.bimap(f, g)

    def bitraverseImpl[G[+_] : Applicative, A, B, C, D](fab: Validation[A, B])(f: (A) => G[C], g: (B) => G[D]) = fab.bitraverse[G, C, D](f, g)
  }

  implicit def validationSemigroup[E, A](implicit E0: Semigroup[E]): Semigroup[Validation[E, A]] = new Semigroup[Validation[E, A]] {
    def append(f1: Validation[E, A], f2: => Validation[E, A]): Validation[E, A] = f1 orElse f2
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

  implicit def validationShow[E: Show, A: Show]: Show[Validation[E, A]] = new Show[Validation[E, A]] {
    def show(f: Validation[E, A]): List[Char] = f match {
      case Success(a) => "Success(".toList ::: Show[A].show(a) ::: ")".toList
      case Failure(e) => "Failure(".toList ::: Show[E].show(e) ::: ")".toList
    }
  }
}

trait ValidationFunctions {
  def success[E, A](a: A): Validation[E, A] = Success(a)

  def failure[E, A](e: E): Validation[E, A] = Failure(e)

  def fromEither[E, A](e: (E \/ A)): Validation[E, A] =
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
