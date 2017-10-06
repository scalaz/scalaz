package scalaz
package syntax
package std

import scalaz.std.{boolean => b}
import scalaz.Tags.{ Conjunction, Disjunction }


final class BooleanOps(self: Boolean) {

  final def conjunction: Boolean @@ Conjunction = Conjunction(self)

  final def disjunction: Boolean @@ Disjunction = Disjunction(self)

  final def |∧| : Boolean @@ Conjunction = conjunction

  final def |/\| : Boolean @@ Conjunction = conjunction

  final def |∨| : Boolean @@ Disjunction = disjunction

  final def |\/| : Boolean @@ Disjunction = disjunction

  /**
   * Conjunction. (AND)
   *
   * {{{
   * p q  p ∧ q
   * 0 0  0
   * 0 1  0
   * 1 0  0
   * 1 1  1
   * }}}
   */
  final def ∧(q: => Boolean): Boolean = b.conjunction(self, q)

  /**
   * Conjunction. (AND)
   *
   * {{{
   * p q  p /\ q
   * 0 0  0
   * 0 1  0
   * 1 0  0
   * 1 1  1
   * }}}
   */
  final def /\(q: => Boolean): Boolean =
    ∧(q)

  /**
   * Disjunction. (OR)
   *
   * {{{
   * p q  p ∨ q
   * 0 0  0
   * 0 1  1
   * 1 0  1
   * 1 1  1
   * }}}
   */
  final def ∨(q: => Boolean): Boolean = b.disjunction(self, q)

  /**
   * Disjunction. (OR)
   *
   * {{{
   * p q  p \/ q
   * 0 0  0
   * 0 1  1
   * 1 0  1
   * 1 1  1
   * }}}
   */
  final def \/(q: => Boolean): Boolean = ∨(q)

  /**
   * Negation of Disjunction. (NOR)
   *
   * {{{
   * p q  p !|| q
   * 0 0  1
   * 0 1  0
   * 1 0  0
   * 1 1  0
   * }}}
   */
  final def !||(q: => Boolean): Boolean = b.nor(self, q)


  /**
   * Negation of Conjunction. (NAND)
   *
   * {{{
   * p q  p !&& q
   * 0 0  1
   * 0 1  1
   * 1 0  1
   * 1 1  0
   * }}}
   */
  final def !&&(q: => Boolean): Boolean = b.nand(self, q)

  /**
   * Conditional.
   *
   * {{{
   * p q  p --> q
   * 0 0  1
   * 0 1  1
   * 1 0  0
   * 1 1  1
   * }}}
   */
  final def -->(q: => Boolean): Boolean = b.conditional(self, q)

  /**
   * Inverse Conditional.
   *
   * {{{
   * p q  p <-- q
   * 0 0  1
   * 0 1  0
   * 1 0  1
   * 1 1  1
   * }}}
   */
  final def <--(q: => Boolean): Boolean = b.inverseConditional(self, q)

  /**
   * Bi-Conditional.
   *
   * {{{
   * p q  p <--> q
   * 0 0  1
   * 0 1  0
   * 1 0  0
   * 1 1  1
   * }}}
   */
  final def <-->(q: => Boolean): Boolean = b.conditional(self, q) && b.inverseConditional(self, q)

  /**
   * Inverse Conditional.
   *
   * {{{
   * p q  p ⇐ q
   * 0 0  1
   * 0 1  0
   * 1 0  1
   * 1 1  1
   * }}}
   */
  final def ⇐(q: => Boolean): Boolean = b.inverseConditional(self, q)

  /**
   * Negation of Conditional.
   *
   * {{{
   * p q  p ⇏ q
   * 0 0  0
   * 0 1  0
   * 1 0  1
   * 1 1  0
   * }}}
   */
  final def ⇏(q: => Boolean): Boolean = b.negConditional(self, q)

  /**
   * Negation of Conditional.
   *
   * {{{
   * p q  p -/> q
   * 0 0  0
   * 0 1  0
   * 1 0  1
   * 1 1  0
   * }}}
   */
  final def -/>(q: => Boolean): Boolean = b.negConditional(self, q)

  /**
   * Negation of Inverse Conditional.
   *
   * {{{
   * p q  p <\- q
   * 0 0  0
   * 0 1  1
   * 1 0  0
   * 1 1  0
   * }}}
   */
  final def ⇍(q: => Boolean): Boolean = b.negInverseConditional(self, q)

  /**
   * Negation of Inverse Conditional.
   *
   * {{{
   * p q  p ⇍ q
   * 0 0  0
   * 0 1  1
   * 1 0  0
   * 1 1  0
   * }}}
   */
  final def <\-(q: => Boolean): Boolean = b.negInverseConditional(self, q)

  /**
   * Executes the given side-effect if this boolean value is `false`.
   */
  final def unless(f: => Unit): Unit = b.unless(self)(f)

  /**
   * Executes the given side-effect if this boolean value is `true`.
   */
  final def when(f: => Unit): Unit = b.when(self)(f)

  /**
   * Returns the given argument if `cond` is `false`, otherwise, unit lifted into M.
   */
  final def unlessM[M[_]: Applicative, A](f: => M[A]): M[Unit] = b.unlessM(self)(f)

  /** A version of `unlessM` that infers the type constructor `M`. */
  final def unlessMU[MA](f: => MA)(implicit M: Unapply[Applicative, MA]): M.M[Unit] = b.unlessMU(self)(f)

  /**
   * Returns the given argument if `cond` is `true`, otherwise, unit lifted into M.
   */
  final def whenM[M[_]: Applicative, A](f: => M[A]): M[Unit] = b.whenM(self)(f)

  /** A version of `whenM` that infers the type constructor `M`. */
  final def whenMU[MA](f: => MA)(implicit M: Unapply[Applicative, MA]): M.M[Unit] = b.whenMU(self)(f)

  /**
   * @return `t` if true, `f` otherwise
   */
  final def fold[A](t: => A, f: => A): A = b.fold(self, t, f)

  final class Conditional[X](t: => X) {
    def |(f: => X) = if (self) t else f
  }

  /**
   * Conditional operator that returns the first argument if this is `true`, the second argument otherwise.
   */
  final def ?[X](t: => X): Conditional[X] = new Conditional(t)

  /**
   * Returns the given argument in `Some` if this is `true`, `None` otherwise.
   */
  final def option[A](a: => A): Option[A] = b.option(self, a)

  /**
   * Returns the given argument in `LazySome` if this is `true`, `LazyNone` otherwise.
   */
  final def lazyOption[A](a: => A): LazyOption[A] = LazyOption.condLazyOption(self, a)

  final class ConditionalEither[A](a: => A) {
    def or[B](b: => B) =
      if (self) \/-(a) else -\/(b)
  }

  /**
   * Returns the first argument in `\/-` if this is `true`, otherwise the second argument in
   * `-\/`.
   */
  final def either[A](a: => A): ConditionalEither[A] = new ConditionalEither(a)

  /**
   * Returns the given argument if this is `true`, otherwise, the zero element for the type of the given
   * argument.
   */
  final def ??[A](a: => A)(implicit z: Monoid[A]): A = b.valueOrZero(self)(a)

  /**
   * Returns the given argument if this is `false`, otherwise, the zero element for the type of the given
   * argument.
   */
  final def !?[A](a: => A)(implicit z: Monoid[A]): A = b.zeroOrValue(self)(a)

  sealed abstract class GuardPrevent[M[_]] {
    def apply[A](a: => A)(implicit M: Applicative[M], M0: PlusEmpty[M]): M[A]
  }

  final def guard[M[_]] = new GuardPrevent[M] {
    def apply[A](a: => A)(implicit M: Applicative[M], M0: PlusEmpty[M]) = b.pointOrEmpty[M, A](self)(a)
  }

  final def prevent[M[_]] = new GuardPrevent[M] {
    def apply[A](a: => A)(implicit M: Applicative[M], M0: PlusEmpty[M]) = b.emptyOrPure[M, A](self)(a)
  }
}

trait ToBooleanOps {
  implicit def ToBooleanOpsFromBoolean(a: Boolean): BooleanOps = new BooleanOps(a)
}
