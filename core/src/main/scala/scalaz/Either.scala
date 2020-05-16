package scalaz

import scala.util.control.NonFatal
import Liskov.<~<

/** Represents a disjunction: a result that is either an `A` or a `B`.
 *
 * An instance of `A` [[\/]] B is either a [[-\/]]`[A]` (aka a "left") or a [[\/-]]`[B]` (aka a "right").
 *
 * A common use of a disjunction is to explicitly represent the possibility of failure in a result as opposed to
 * throwing an exception. By convention, the left is used for errors and the right is reserved for successes.
 * For example, a function that attempts to parse an integer from a string may have a return type of
 * `NumberFormatException` [[\/]] `Int`. However, since there is no need to actually throw an exception, the type (`A`)
 * chosen for the "left" could be any type representing an error and has no need to actually extend `Exception`.
 *
 * `A` [[\/]] `B` is isomorphic to `scala.Either[A, B]`, but [[\/]] is right-biased for all Scala versions, so methods
 * such as `map` and `flatMap` apply only in the context of the "right" case. This right bias makes [[\/]] more
 * convenient to use than `scala.Either` in a monadic context in Scala versions <2.12. Methods such as `swap`,
 * `swapped`, and `leftMap` provide functionality that `scala.Either` exposes through left projections.
 *
 * `A` [[\/]] `B` is also isomorphic to [[Validation]]`[A, B]`. The subtle but important difference is that [[Applicative]]
 * instances for [[Validation]] accumulates errors ("lefts") while [[Applicative]] instances for [[\/]] fail fast on the
 * first "left" they evaluate. This fail-fast behavior allows [[\/]] to have lawful [[Monad]] instances that are consistent
 * with their [[Applicative]] instances, while [[Validation]] cannot.
 */
sealed abstract class \/[A, B] extends Product with Serializable {
  final class SwitchingDisjunction[X](r: => X) {
    def <<?:(left: X): X =
      foldConst(left, r)
  }

  /** If this disjunction is right, return the given X value, otherwise, return the X value given to the return value. */
  def :?>>[X](right: => X): SwitchingDisjunction[X] =
    new SwitchingDisjunction[X](right)

  /** Return `true` if this disjunction is left. */
  def isLeft: Boolean =
    this match {
      case -\/(_) => true
      case \/-(_) => false
    }

  /** Return `true` if this disjunction is right. */
  def isRight: Boolean =
    this match {
      case -\/(_) => false
      case \/-(_) => true
    }

  /** Catamorphism. Run the first given function if left, otherwise, the second given function. */
  def fold[X](l: A => X, r: B => X): X =
    this match {
      case -\/(a) => l(a)
      case \/-(b) => r(b)
    }

  /** Evaluate `l` and return if left, otherwise, `r`. */
  def foldConst[X](l: => X, r: => X): X =
    this match {
      case -\/(a) => l
      case \/-(b) => r
    }

  /** Spin in tail-position on the right value of this disjunction. */
  def loopr[X](left: A => X, right: B => X \/ (A \/ B)): X =
    \/.loopRight(this, left, right)

  /** Spin in tail-position on the left value of this disjunction. */
  def loopl[X](left: A => X \/ (A \/ B), right: B => X): X =
    \/.loopLeft(this, left, right)

  /** Flip the left/right values in this disjunction. Alias for `unary_~` */
  def swap: (B \/ A) =
    this match {
      case -\/(a) => \/-(a)
      case \/-(b) => -\/(b)
    }

  /** Flip the left/right values in this disjunction. Alias for `swap` */
  def unary_~ : (B \/ A) =
    swap

  /** Run the given function on this swapped value. Alias for `~` */
  def swapped[AA, BB](k: (B \/ A) => (BB \/ AA)): (AA \/ BB) =
    k(swap).swap

  /** Run the given function on this swapped value. Alias for `swapped` */
  def ~[AA, BB](k: (B \/ A) => (BB \/ AA)): (AA \/ BB) =
    swapped(k)

  /** Binary functor map on this disjunction. */
  def bimap[C, D](f: A => C, g: B => D): (C \/ D) =
    this match {
      case -\/(a) => -\/(f(a))
      case \/-(b) => \/-(g(b))
    }

  /** Run the given function on the left value. */
  def leftMap[C](f: A => C): (C \/ B) =
    this match {
      case -\/(a) => -\/(f(a))
      case b @ \/-(_) => b.coerceLeft
    }

  /** Binary functor traverse on this disjunction. */
  def bitraverse[F[_]: Functor, C, D](f: A => F[C], g: B => F[D]): F[C \/ D] =
    this match {
      case -\/(a) => Functor[F].map(f(a))(\/.left)
      case \/-(b) => Functor[F].map(g(b))(\/.right)
    }

  /** Map on the right of this disjunction. */
  def map[D](g: B => D): (A \/ D) =
    this match {
      case \/-(b)     => \/-(g(b))
      case a @ -\/(_) => a.coerceRight
    }

  /** Traverse on the right of this disjunction. */
  def traverse[F[_]: Applicative, D](g: B => F[D]): F[A \/ D] =
    this match {
      case a @ -\/(_) => Applicative[F].point(a.coerceRight)
      case \/-(b) => Functor[F].map(g(b))(\/.right)
    }

  /** Run the side-effect on the right of this disjunction. */
  def foreach(g: B => Unit): Unit =
    fold(_ => (), g)

  /** Apply a function in the environment of the right of this disjunction. */
  def ap[C](f: => A \/ (B => C)): (A \/ C) =
    f flatMap (ff => map(ff(_)))

  /** Bind through the right of this disjunction. */
  def flatMap[D](g: B => (A \/ D)): (A \/ D) =
    this match {
      case a @ -\/(_) => a.coerceRight
      case \/-(b) => g(b)
    }

  /** Fold on the right of this disjunction. */
  def foldRight[Z](z: => Z)(f: (B, => Z) => Z): Z =
    this match {
      case -\/(_) => z
      case \/-(a) => f(a, z)
    }

  /** Filter on the right of this disjunction. */
  def filter(p: B => Boolean)(implicit M: Monoid[A]): (A \/ B) =
    this match {
      case -\/(_) => this
      case \/-(b) => if(p(b)) this else -\/(M.zero)
    }

  /** Return `true` if this disjunction is a right value satisfying the given predicate. */
  def exists[BB >: B](p: BB => Boolean): Boolean =
    this match {
      case -\/(_) => false
      case \/-(b) => p(b)
    }

  /** Return `true` if this disjunction is a left value or the right value satisfies the given predicate. */
  def forall[BB >: B](p: BB => Boolean): Boolean =
    this match {
      case -\/(_) => true
      case \/-(b) => p(b)
    }

  /** Return an empty list or list with one element on the right of this disjunction. */
  def toList: List[B] =
    this match {
      case -\/(_) => Nil
      case \/-(b) => b :: Nil
    }

  /** Return an empty list or list with one element on the right of this disjunction. */
  def toIList[BB >: B]: IList[BB] =
    this match {
      case -\/(_) => INil()
      case \/-(b) => b :: INil()
    }


  /** Return an empty stream or stream with one element on the right of this disjunction. */
  def toStream: Stream[B] =
    this match {
      case -\/(_) => Stream()
      case \/-(b) => Stream(b)
    }

  /** Return an empty option or option with one element on the right of this disjunction. Useful to sweep errors under the carpet. */
  def toOption: Option[B] =
    this match {
      case -\/(_) => None
      case \/-(b) => Some(b)
    }

  /** Return an empty maybe or option with one element on the right of this disjunction. Useful to sweep errors under the carpet. */
  def toMaybe[BB >: B]: Maybe[BB] =
    this match {
      case -\/(_) => Maybe.empty
      case \/-(b) => Maybe.just(b)
    }

  /** Convert to a core `scala.Either` at your own peril. */
  def toEither: Either[A, B] =
    this match {
      case -\/(a) => Left(a)
      case \/-(b) => Right(b)
    }

  /** Return the right value of this disjunction or the given default if left. Alias for `|` */
  def getOrElse[BB >: B](x: => BB): BB =
    this match {
      case -\/(_) => x
      case \/-(b) => b
    }

  /** Return the right value of this disjunction or the given default if left. Alias for `getOrElse` */
  def |[BB >: B](x: => BB): BB =
    getOrElse(x)

  /** Return the right value of this disjunction or run the given function on the left. */
  def valueOr[BB >: B](x: A => BB): BB =
    this match {
      case -\/(a) => x(a)
      case \/-(b) => b
    }

  /** Return this if it is a right, otherwise, return the given value. Alias for `|||` */
  def orElse[C](x: => C \/ B): C \/ B =
    this match {
      case -\/(_) => x
      case right@ \/-(_) => right.coerceLeft
    }

  /** Return this if it is a right, otherwise, return the given value. Alias for `orElse` */
  def |||[C](x: => C \/ B): C \/ B =
    orElse(x)

  /**
   * Sums up values inside disjunction, if both are left or right. Returns first left otherwise.
   * {{{
   * \/-(v1) +++ \/-(v2) → \/-(v1 + v2)
   * \/-(v1) +++ -\/(v2) → -\/(v2)
   * -\/(v1) +++ \/-(v2) → -\/(v1)
   * -\/(v1) +++ -\/(v2) → -\/(v1 + v2)
   * }}}
   */
  def +++(x: => A \/ B)(implicit M1: Semigroup[B], M2: Semigroup[A]): A \/ B =
    this match {
      case -\/(a1) => x match {
        case -\/(a2) => -\/(M2.append(a1, a2))
        case \/-(_) => this
      }
      case \/-(b1) => x match {
        case b2 @ -\/(_) => b2
        case \/-(b2) => \/-(M1.append(b1, b2))
      }
    }

  /** Ensures that the right value of this disjunction satisfies the given predicate, or returns left with the given value. */
  def ensure(onLeft: => A)(f: B => Boolean): (A \/ B) = this match {
    case \/-(b) => if (f(b)) this else -\/(onLeft)
    case -\/(_) => this
  }

  /** Run the given function on the left and return right with the result. */
  def recover(pf: PartialFunction[A, B]): (A \/ B) = this match {
    case -\/(a) if (pf isDefinedAt a) => \/-(pf(a))
    case _ => this
  }

  /** Run the given function on the left and return the result. */
  def recoverWith(pf: PartialFunction[A, A \/ B]): (A \/ B) = this match {
    case -\/(a) if (pf isDefinedAt a) => pf(a)
    case _ => this
  }

  /** Compare two disjunction values for equality. */
  def ===[AA >: A, BB >: B](x: AA \/ BB)(implicit EA: Equal[AA], EB: Equal[BB]): Boolean =
    this match {
      case -\/(a1) => x match {
        case -\/(a2) => Equal[AA].equal(a1, a2)
        case \/-(_) => false
      }
      case \/-(b1) => x match {
        case \/-(b2) => Equal[BB].equal(b1, b2)
        case -\/(_) => false
      }
    }

  /** Compare two disjunction values for ordering. */
  def compare[AA >: A, BB >: B](x: AA \/ BB)(implicit EA: Order[AA], EB: Order[BB]): Ordering =
    this match {
      case -\/(a1) => x match {
        case -\/(a2) => Order[AA].apply(a1, a2)
        case \/-(_) => Ordering.LT
      }
      case \/-(b1) => x match {
        case \/-(b2) => Order[BB].apply(b1, b2)
        case -\/(_) => Ordering.GT
      }
    }

  import syntax.show._
  /** Show for a disjunction value. */
  def show[AA >: A, BB >: B](implicit SA: Show[AA], SB: Show[BB]): Cord =
    this match {
      case -\/(a) => cord"-\\/(${SA.show(a)})"
      case \/-(b) => cord"\\/-(${SB.show(b)})"
    }

  /** Convert to a Validation. */
  def toValidation: Validation[A, B] =
    this match {
      case -\/(a) => Failure(a)
      case \/-(b) => Success(b)
    }

  /** Convert to a ValidationNel. */
  def toValidationNel[AA>:A] : ValidationNel[AA,B] =
    this match {
      case -\/(a) => Failure(NonEmptyList(a))
      case \/-(b) => Success(b)
    }

  /** Run a validation function and back to disjunction again. Alias for `@\?/` */
  def validationed[AA, BB](k: Validation[A, B] => Validation[AA, BB]): AA \/ BB =
    k(toValidation).toDisjunction

  /** Run a validation function and back to disjunction again. Alias for `validationed` */
  def @\?/[AA, BB](k: Validation[A, B] => Validation[AA, BB]): AA \/ BB =
    validationed(k)

  /** Return the value from whichever side of the disjunction is defined, given a commonly assignable type. */
  def merge[AA >: A](implicit ev: B <~< AA): AA =
    this match {
      case -\/(a) => a
      case \/-(b) => ev(b)
    }

  /** Convert to a These. */
  def toThese: A \&/ B =
    fold(
      a => \&/.This(a),
      b => \&/.That(b)
    )

  def orRaiseError[F[_]](implicit F: MonadError[F, A]): F[B] =
    fold(
      a => F.raiseError(a),
      b => F.point(b)
    )
}

/** A left disjunction
 *
 * Often used to represent the failure case of a result
 */
final case class -\/[A, B](a: A) extends (A \/ B) {
  def coerceRight[C]: A \/ C = this.asInstanceOf[A \/ C]
}

object -\/ {
  /** Override smart constructor to return less specific type. */
  def apply[A, B](a: A): A \/ B = new -\/(a)
}

/** A right disjunction
 *
 * Often used to represent the success case of a result
 */
final case class \/-[A, B](b: B) extends (A \/ B) {
  def coerceLeft[C]: C \/ B = this.asInstanceOf[C \/ B]
}

object \/- {
  /** Override smart constructor to return less specific type. */
  def apply[A, B](b: B): A \/ B = new \/-(b)
}

object \/ extends DisjunctionInstances {

  /** Construct a left disjunction value. */
  def left[A, B]: A => A \/ B =
    -\/(_)

  /** Construct a right disjunction value. */
  def right[A, B]: B => A \/ B =
    \/-(_)

  /** Construct a left disjunction value but specify only the `right` type param
   *
   * @example {{{
   *   val x = \/.l[String](42)
   *   x: Int \/ String
   * }}}
   */
  def l[B]: L[B] =
    new L[B]

  private[scalaz] final class L[B] private[scalaz] (private val dummy: Boolean = true) extends AnyVal {
    def apply[A](left: A): A \/ B = -\/(left)
  }

  /** Construct a right disjunction value but specify only the `left` type param
   *
   * @example {{{
   *   val x = \/.r[String](42)
   *   x: String \/ Int
   * }}}
   */
  def r[B]: R[B] =
    new R[B]

  private[scalaz] final class R[A] private[scalaz] (private val dummy: Boolean = true) extends AnyVal {
    def apply[B](right: B): A \/ B = \/-(right)
  }

  /** Construct a disjunction value from a standard `scala.Either`. */
  def fromEither[A, B](e: Either[A, B]): A \/ B =
    e fold (left, right)

  /** Construct a disjunction value from a standard `scala.Option`. */
  def fromOption[A, B](ifNone: => A)(o: Option[B]): A \/ B =
    o.fold(left[A, B](ifNone))(right)

  /**
   * Wrap a call to a deterministic partial function, making a total function.
   * May be used to interface with legacy methods that do not have an FP
   * equivalent.
   *
   * The `err` callback must convert the non-referentially transparent
   * `Throwable` (which is anything caught by the `NonFatal` construct) into a
   * data type. The caller is trusted not to allow the stack trace to escape
   * into the `A` data type.
   *
   * Note that exceptions are extremely inefficient. Callers should consider
   * validating the input to their partial function and exiting early.
   *
   * If no useful information can be obtained from the `Throwable`, prefer
   * [[scalaz.Maybe#attempt]].
   *
   * For interfacing with non-deterministic blocks of code that may or may not
   * throw `Throwable`, use [[scalaz.effect.IO]].
   *
   * For interfacing with deterministic functions that violate the type system
   * by returning `null`, use [[scalaz.Maybe#fromNullable]].
   */
  def attempt[A, B](f: => B)(err: Throwable => A): A \/ B =
    try \/-(f) catch {
      case NonFatal(t) => -\/(err(t))
    }

  /** Spin in tail-position on the right value of the given disjunction. */
  @annotation.tailrec
  final def loopRight[A, B, X](d: A \/ B, left: A => X, right: B => X \/ (A \/ B)): X =
    d match {
      case -\/(a) => left(a)
      case \/-(b) => right(b) match {
        case -\/(x) => x
        case \/-(q) => loopRight(q, left, right)
      }
    }

  /** Spin in tail-position on the left value of the given disjunction. */
  @annotation.tailrec
  final def loopLeft[A, B, X](d: A \/ B, left: A => X \/ (A \/ B), right: B => X): X =
    d match {
      case -\/(a) => left(a) match {
        case -\/(x) => x
        case \/-(q) => loopLeft(q, left, right)
      }
      case \/-(b) => right(b)
    }

}

sealed abstract class DisjunctionInstances extends DisjunctionInstances0 {
  implicit def DisjunctionOrder[A: Order, B: Order]: Order[A \/ B] =
    new Order[A \/ B] {
      def order(a1: A \/ B, a2: A \/ B) =
        a1 compare a2
      override def equal(a1: A \/ B, a2: A \/ B) =
        a1 === a2
    }

  implicit def DisjunctionMonoid[A: Semigroup, B: Monoid]: Monoid[A \/ B] =
    new Monoid[A \/ B] {
      def append(a1: A \/ B, a2: => A \/ B) =
        a1 +++ a2
      def zero =
        \/-(Monoid[B].zero)
    }
}

sealed abstract class DisjunctionInstances0 extends DisjunctionInstances1 {
  implicit def DisjunctionEqual[A: Equal, B: Equal]: Equal[A \/ B] =
    new Equal[A \/ B] {
      def equal(a1: A \/ B, a2: A \/ B) =
        a1 === a2
    }

  implicit def DisjunctionShow[A: Show, B: Show]: Show[A \/ B] =
    Show.show(_.show)

  implicit def DisjunctionSemigroup[A: Semigroup, B: Semigroup]: Semigroup[A \/ B] =
    new Semigroup[A \/ B] {
      def append(a1: A \/ B, a2: => A \/ B) =
        a1 +++ a2
    }
}

sealed abstract class DisjunctionInstances1 extends DisjunctionInstances2 {
  implicit def DisjunctionBand[A: Band, B: Band]: Band[A \/ B] =
    new Band[A \/ B] {
      def append(a1: A \/ B, a2: => A \/ B) =
        a1 +++ a2
    }

  implicit def DisjunctionInstances1[L]: Traverse[\/[L, *]] with Monad[\/[L, *]] with BindRec[\/[L, *]] with Cozip[\/[L, *]] with Plus[\/[L, *]] with Alt[\/[L, *]] with Optional[\/[L, *]] with MonadError[\/[L, *], L] =
    new Traverse[\/[L, *]] with Monad[\/[L, *]] with BindRec[\/[L, *]] with Cozip[\/[L, *]] with Plus[\/[L, *]] with Alt[\/[L, *]] with Optional[\/[L, *]] with MonadError[\/[L, *], L] {
      override def map[A, B](fa: L \/ A)(f: A => B) =
        fa map f

      override def ap[A,B](fa: => L \/ A)(f: => L \/ (A => B)): L \/ B = fa.ap(f)

      override def apply2[A, B, C](fa: => L \/ A, fb: => L \/ B)(f: (A, B) => C): L \/ C =
        fa match {
          case \/-(a) =>
            fb match {
              case \/-(b) => \/-(f(a, b))
              case e => e.asInstanceOf[L \/ C]
            }
          case e => e.asInstanceOf[L \/ C]
        }

      @scala.annotation.tailrec
      def tailrecM[A, B](a: A)(f: A => L \/ (A \/ B)): L \/ B =
        f(a) match {
          case l @ -\/(_) => l.coerceRight
          case \/-(-\/(a0)) => tailrecM(a0)(f)
          case \/-(rb @ \/-(_)) => rb.coerceLeft
        }

      def bind[A, B](fa: L \/ A)(f: A => L \/ B) =
        fa flatMap f

      override def emap[A, B](fa: L \/ A)(f: A => L \/ B) = bind(fa)(f)

      def point[A](a: => A) =
        \/-(a)

      def traverseImpl[G[_] : Applicative, A, B](fa: L \/ A)(f: A => G[B]) =
        fa.traverse(f)

      override def foldRight[A, B](fa: L \/ A, z: => B)(f: (A, => B) => B) =
        fa.foldRight(z)(f)

      def cozip[A, B](x: L \/ (A \/ B)) =
        x match {
          case l @ -\/(_) => -\/(l.coerceRight)
          case \/-(e) => e match {
            case -\/(a) => -\/(\/-(a))
            case b @ \/-(_) => \/-(b.coerceLeft)
          }
        }

      def plus[A](a: L \/ A, b: => L \/ A) =
        a orElse b

      def alt[A](a: => L \/ A, b: => L \/ A) =
        plus(a, b)

      def pextract[B, A](fa: L \/ A): (L \/ B) \/ A = fa match {
        case l@ -\/(_) => -\/(l.coerceRight)
        case r@ \/-(_) => r.coerceLeft
      }

      def raiseError[A](e: L): L \/ A =
        -\/(e)

      def handleError[A](fa: L \/ A)(f: L => L \/ A): L \/ A = fa match {
        case -\/(e) => f(e)
        case r => r
      }
    }
}

sealed abstract class DisjunctionInstances2 {
  implicit val DisjunctionInstances2 : Bitraverse[\/] = new Bitraverse[\/] {
    override def bimap[A, B, C, D](fab: A \/ B)
                                  (f: A => C, g: B => D) = fab bimap (f, g)

    def bitraverseImpl[G[_] : Applicative, A, B, C, D](fab: A \/ B)
                                                  (f: A => G[C], g: B => G[D]) =
      fab.bitraverse(f, g)
  }

  implicit val DisjunctionAssociative: Associative[\/] = new Associative[\/] {
    def reassociateLeft[A, B, C](f: \/[A, \/[B, C]]) =
      f.fold(
        a => -\/(-\/(a)),
        _.fold(
          b => -\/(\/-(b)),
          \/.right
        )
      )

    def reassociateRight[A, B, C](f: \/[\/[A, B], C]) =
      f.fold(
        _.fold(
          \/.left,
          b => \/-(-\/(b))
        ),
        c => \/-(\/-(c))
      )
  }
}
