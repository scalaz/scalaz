package scalaz
package syntax
package std

import scalaz.std.boolean
import scalaz.std.anyVal._


trait BooleanV extends SyntaxV[Boolean] {

  final def conjunction: Boolean @@ Conjunction = Tag(self)

  final def |∧| : Boolean @@ Conjunction = conjunction

  final def |/\| : Boolean @@ Conjunction = conjunction

  /**
   * Conjunction. (AND)
   *
   * <pre>
   * p q  p ∧ q
   * 0 0  0
   * 0 1  0
   * 1 0  0
   * 1 1  1
   * </pre>
   */
  final def ∧(q: => Boolean) = boolean.conjunction(self, q)

  /**
   * Conjunction. (AND)
   *
   * <pre>
   * p q  p /\ q
   * 0 0  0
   * 0 1  0
   * 1 0  0
   * 1 1  1
   * </pre>
   */
  final def /\(q: => Boolean) =
    ∧(q)

  /**
   * Disjunction. (OR)
   *
   * <pre>
   * p q  p ∨ q
   * 0 0  0
   * 0 1  1
   * 1 0  1
   * 1 1  1
   * </pre>
   */
  final def ∨(q: => Boolean): Boolean = boolean.disjunction(self, q)

  /**
   * Disjunction. (OR)
   *
   * <pre>
   * p q  p \/ q
   * 0 0  0
   * 0 1  1
   * 1 0  1
   * 1 1  1
   * </pre>
   */
  final def \/(q: => Boolean): Boolean = ∨(q)

  /**
   * Negation of Conjunction. (NOR)
   *
   * <pre>
   * p q  p !&& q
   * 0 0  1
   * 0 1  1
   * 1 0  1
   * 1 1  0
   * </pre>
   */
  final def !&&(q: => Boolean) = boolean.nor(self, q)

  /**
   * Negation of Disjunction. (NAND)
   *
   * <pre>
   * p q  p !|| q
   * 0 0  1
   * 0 1  0
   * 1 0  0
   * 1 1  0
   * </pre>
   */
  final def !||(q: => Boolean) = boolean.nand(self, q)

  /**
   * Conditional.
   *
   * <pre>
   * p q  p --> q
   * 0 0  1
   * 0 1  1
   * 1 0  0
   * 1 1  1
   * </pre>
   */
  final def -->(q: => Boolean) = boolean.conditional(self, q)

  /**
   * Inverse Conditional.
   *
   * <pre>
   * p q  p <-- q
   * 0 0  1
   * 0 1  0
   * 1 0  1
   * 1 1  1
   * </pre>
   */
  final def <--(q: => Boolean) = boolean.inverseConditional(self, q)

  /**
   * Negational of Conditional.
   *
   * <pre>
   * p q  p ⇏ q
   * 0 0  0
   * 0 1  0
   * 1 0  1
   * 1 1  0
   * </pre>
   */
  final def ⇏(q: => Boolean) = boolean.negConditional(self, q)

  /**
   * Negational of Conditional.
   *
   * <pre>
   * p q  p -/> q
   * 0 0  0
   * 0 1  0
   * 1 0  1
   * 1 1  0
   * </pre>
   */
  final def -/>(q: => Boolean) = boolean.negConditional(self, q)

  /**
   * Negation of Inverse Conditional.
   *
   * <pre>
   * p q  p <\- q
   * 0 0  0
   * 0 1  1
   * 1 0  0
   * 1 1  0
   * </pre>
   */
  final def ⇍(q: => Boolean) = boolean.negInverseConditional(self, q)

  /**
   * Negation of Inverse Conditional.
   *
   * <pre>
   * p q  p ⇍ q
   * 0 0  0
   * 0 1  1
   * 1 0  0
   * 1 1  0
   * </pre>
   */
  final def <\-(q: => Boolean) = boolean.negInverseConditional(self, q)

  /**
   * Executes the given side-effect if this boolean value is <code>false</code>.
   */
  final def unless(f: => Unit) = boolean.unless(self)(f)

  /**
   * Executes the given side-effect if this boolean value is <code>true</code>.
   */
  final def when(f: => Unit) = boolean.when(self)(f)

  /**
   * @return `t` if true, `f` otherwise
   */
  final def fold[A](t: => A, f: => A): A = boolean.fold(self, t, f)

  trait Conditional[X] {
    def |(f: => X): X
  }

  /**
   * Conditional operator that returns the first argument if this is `true`, the second argument otherwise.
   */
  final def ?[X](t: => X): Conditional[X] = new Conditional[X] {
    def |(f: => X) = if (self) t else f
  }

  /**
   * Returns the given argument in <code>Some</code> if this is <code>true</code>, <code>None</code> otherwise.
   */
  final def option[A](a: => A): Option[A] = boolean.option(self, a)

  /**
   * Returns the given argument in <code>lazySome</code> if this is <code>true</code>, <code>lazyNone</code> otherwise.
   */
  final def lazyOption[A](a: => A): LazyOption[A] = LazyOption.condLazyOption(self, a)

  trait ConditionalEither[A] {
    def or[B](b: => B): Either[A, B]
  }

  /**
   * Returns the first argument in <code>Left</code> if this is <code>true</code>, otherwise the second argument in
   * <code>Right</code>.
   */
  final def either[A, B](a: => A) = new ConditionalEither[A] {
    def or[B](b: => B) =
      if (self) Left(a) else Right(b)
  }

  /**
   * Returns the given argument if this is <code>true</code>, otherwise, the zero element for the type of the given
   * argument.
   */
  final def ??[A](a: => A)(implicit z: Monoid[A]): A = boolean.valueOrZero(self)(a)

  /**
   * Returns the given argument if this is <code>false</code>, otherwise, the zero element for the type of the given
   * argument.
   */
  final def !?[A](a: => A)(implicit z: Monoid[A]): A = boolean.zeroOrValue(self)(a)

  trait GuardPrevent[M[_]] {
    def apply[A](a: => A)(implicit M: Pointed[M], M0: Empty[M]): M[A]
  }

  final def guard[M[_]] = new GuardPrevent[M] {
    def apply[A](a: => A)(implicit M: Pointed[M], M0: Empty[M]) = boolean.pureOrEmpty[M, A](self)(a)
  }

  final def prevent[M[_]] = new GuardPrevent[M] {
    def apply[A](a: => A)(implicit M: Pointed[M], M0: Empty[M]) = boolean.emptyOrPure[M, A](self)(a)
  }
}

trait ToBooleanV {
  implicit def ToBooleanVFromBoolean(a: Boolean): BooleanV = new BooleanV {
    val self = a
  }
}
