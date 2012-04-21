package scalaz
package syntax
package std

import scalaz.std.option
import scalaz.Tags.{Last, First}

trait OptionOps[A] extends Ops[Option[A]] {
  final def cata[X](some: A => X, none: => X): X = option.cata(self)(some, none)
  final def fold[X](some: A => X, none: => X): X = cata(some, none)

  sealed trait Fold[X] {
    def none(s: => X): X
  }

  /**
   * Returns the provided function `s` applied to item contained in the Option if it is defined,
   * otherwise, the provided value `n`.
   * <p/>
   * This is a syntactic alternative to  { @link scalaz.OptionW # cata }
   * <p/>
   * Example:
   * <code>
   * option.some(_ * 2).none(0)
   * </code>
   */
  final def some[X](s: A => X): Fold[X] = new Fold[X] {
    def none(n: => X): X = cata(s, n)
  }

  sealed trait Conditional[X] {
    def |(n: => X): X
  }

  /**
   * Ternary operator. Note that the arguments s and n are call-by-name.
   * <p/>
   * Example
   * <code>
   * option ? "defined" | "undefined"
   * </code>
   */
  final def ?[X](s: => X): Conditional[X] = new Conditional[X] {
    def |(n: => X): X = self match {
      case None    => n
      case Some(_) => s
    }
  }


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
   * val o: Option = None
   * val a: List[String] = ~o
   * }}}
   */
  final def unary_~(implicit z: Monoid[A]): A = self getOrElse z.zero

  final def toSuccess[E](e: => E): Validation[E, A] = option.toSuccess(self)(e)

  final def toFailure[B](b: => B): Validation[A, B] = option.toFailure(self)(b)

  final def first: Option[A] @@ First = Tag(self)

  final def last: Option[A] @@ Last = Tag(self)

  final def orEmpty[M[_] : Pointed : PlusEmpty]: M[A] = option.orEmpty[A, M](self)

  final def foldLift[F[_] : Pointed, B](b: => B, k: F[A] => B): B = option.foldLift(self)(b, k)

  final def foldLiftOpt[B](b: => B, k: Option[A] => B): B = option.foldLiftOpt[A, B](self)(b, k)
}

trait ToOptionOps {
  implicit def ToOptionOpsFromOption[A](a: Option[A]): OptionOps[A] = new OptionOps[A] {
    val self = a
  }
}
