package scalaz
package syntax
package std

import scalaz.std.{option => o}
import scalaz.Tags.{Last, First}

final class OptionOps[A](self: Option[A]) {
  final def cata[X](some: A => X, none: => X): X = o.cata(self)(some, none)

  final class Fold[X](s: A => X) {
    def none(n: => X): X = cata(s, n)
  }

  /**
   * Returns the provided function `s` applied to item contained in the Option if it is defined,
   * otherwise, the provided value `n`.
   * <p/>
   * This is a syntactic alternative to [[scalaz.syntax.std.OptionOps#cata]]
   * <p/>
   * Example:
   * {{{
   * o.some(_ * 2).none(0)
   * }}}
   */
  final def some[X](s: A => X): Fold[X] = new Fold(s)

  final class Conditional[X](s: => X) {
    def |(n: => X): X = self match {
      case None    => n
      case Some(_) => s
    }
  }

  /**
   * Ternary operator. Note that the arguments s and n are call-by-name.
   * <p/>
   * Example
   * {{{
   * option ? "defined" | "undefined"
   * }}}
   */
  final def ?[X](s: => X): Conditional[X] = new Conditional(s)

  /**
   * Executes the provided side effect if the Option if it is undefined.
   */
  final def ifNone(n: => Unit): Unit = if (self.isEmpty) n

  /**
   * Returns the item contained in the Option if it is defined, otherwise, raises an error with the provided message.
   */
  final def err(message: => String): A = self.getOrElse(sys.error(message))

  /**
   * Returns the item contained in the Option if it is defined, otherwise, the provided argument.
   */
  final def |(a: => A): A = self getOrElse a

  /**
   * Returns the item contained in the Option if it is defined, otherwise, the zero element for the type A
   * <p/>
   * For example:
   * {{{
   * val o: Option[List[String]] = None
   * val a: List[String] = ~o // List()
   * }}}
   */
  final def unary_~(implicit z: Monoid[A]): A = self getOrElse z.zero

  final def orZero(implicit z: Monoid[A]): A = self getOrElse z.zero

  final def toSuccess[E](e: => E): Validation[E, A] = o.toSuccess(self)(e)

  final def toFailure[B](b: => B): Validation[A, B] = o.toFailure(self)(b)

  final def toSuccessNel[E](e: => E): ValidationNel[E, A] = o.toSuccessNel(self)(e)

  final def toFailureNel[B](b: => B): ValidationNel[A, B] = o.toFailureNel(self)(b)

  final def toRightDisjunction[E](e: => E): E \/ A = o.toRight(self)(e)

  final def toLeftDisjunction[B](b: => B): A \/ B = o.toLeft(self)(b)

  final def \/>[E](e: => E): E \/ A = o.toRight(self)(e)

  final def <\/[B](b: => B): A \/ B = o.toLeft(self)(b)

  final def first: Option[A] @@ First = Tag(self)

  final def last: Option[A] @@ Last = Tag(self)

  final def orEmpty[M[_] : Applicative : PlusEmpty]: M[A] = o.orEmpty[A, M](self)

  final def getOrElseF[F[_]: Applicative](fa: => F[A]): F[A] = o.getOrElseF[A, F](self)(fa)

  final def foldLift[F[_] : Applicative, B](b: => B, k: F[A] => B): B = o.foldLift(self)(b, k)

  final def foldLiftOpt[B](b: => B, k: Option[A] => B): B = o.foldLiftOpt[A, B](self)(b, k)

  final def toMaybe: Maybe[A] = o.toMaybe(self)
}

trait ToOptionOps {
  implicit def ToOptionOpsFromOption[A](a: Option[A]): OptionOps[A] = new OptionOps(a)
}
