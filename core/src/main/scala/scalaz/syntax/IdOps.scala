package scalaz
package syntax

import annotation.tailrec

final class IdOps[A](val self: A) extends AnyVal {
  /**Returns `self` if it is non-null, otherwise returns `d`. */
  final def ??(d: => A)(implicit ev: Null <:< A): A =
    if (self == null) d else self

  /**Applies `self` to the provided function. The Thrush combinator. */
  final def |>[B](f: A => B): B =
    f(self)

  /**Applies `self` to the provided function. The Thrush combinator. */
  final def ▹[B](f: A => B): B =
    f(self)

  /**Applies `self` to the provide function for its side effect, and returns `self`. The Kestrel combinator.
   * Mostly for use with dodgy libraries that give you values that need additional initialization or
   * mutation before they're valid to use.
   *
   * The name `tap` comes from the Ruby method: [[http://ruby-doc.org/core-2.0.0/Object.html#method-i-tap]]
   * which allows you to "tap into" a method call chain, in order to perform operations on intermediate
   * results within the chain.  `unsafe` because it enables side effects.
   */
  @deprecated("will be removed in 7.3", "7.2")
  final def unsafeTap(f: A => Any): A = {
    f(self); self
  }

  /** Alias for `unsafeTap`. */
  @deprecated("will be removed in 7.3", "7.2")
  final def <|(f: A => Any): A = unsafeTap(f)

  /** Alias for `unsafeTap`. */
  @deprecated("will be removed in 7.3", "7.2")
  final def ◃(f: A => Any): A = unsafeTap(f)

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
