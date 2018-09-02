package scalaz
package syntax

import annotation.tailrec

final class IdOps[A](private val self: A) extends AnyVal {
  /**Returns `self` if it is non-null, otherwise returns `d`. */
  final def ??(d: => A)(implicit ev: Null <:< A): A =
    if (self == ev(null)) d else self

  /**Applies `self` to the provided function. The Thrush combinator. */
  final def |>[B](f: A => B): B =
    f(self)

  /**Applies `self` to the provided function. The Thrush combinator. */
  final def ▹[B](f: A => B): B =
    f(self)

  /** Applies `self` to the provided function if the predicate is satisfied, otherwise return self. */
  @inline def ?|>(p: A => Boolean, f: A => A): A = if (p(self)) f(self) else self

  /** Alternative syntax for the Thrush combinator or a total `match`. */
  @inline final def into[B](f: A => B): B = f(self)

  /** Applies `self` to the provided function if the predicate is satisfied, otherwise return self. */
  @inline def applyIf(p: A => Boolean)(f: A => A): A = if (p(self)) f(self) else self

  final def squared: (A, A) =
    (self, self)

  /**
   * @return the result of pf(value) if defined, otherwise the Zero element of type B.
   */
  def matchOrZero[B: Monoid](pf: PartialFunction[A, B]): B =
    pf.lift(self) match {
      case None    => Monoid[B].zero
      case Some(x) => x
    }

  /** Repeatedly apply `f`, seeded with `self`, checking after each iteration whether the predicate `p` holds. */
  final def doWhile(f: A => A, p: A => Boolean): A = {
    @tailrec
    def loop(value: A): A = {
      val x = f(value)
      if (p(x)) loop(x) else x
    }
    loop(self)
  }

  /** Repeatedly apply `f`, seeded with `self`, checking before each iteration whether the predicate `p` holds. */
  final def whileDo(f: A => A, p: A => Boolean): A = {
    @tailrec
    def loop(value: A): A = {
      if (p(value)) loop(f(value)) else value
    }
    loop(self)
  }

  /**
   * If the provided partial function is defined for `self` run this,
   * otherwise lift `self` into `F` with the provided [[scalaz.Applicative]].
   */
  def visit[F[_] : Applicative](p: PartialFunction[A, F[A]]): F[A] =
    if (p isDefinedAt self) p(self)
    else Applicative[F].point(self)
}

trait ToIdOps {
  implicit def ToIdOps[A](a: A): IdOps[A] = new IdOps(a)
}
