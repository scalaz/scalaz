package scalaz
package wrap

sealed trait BooleanW {

  import data._, LazyEither._
  import newtypes._

  val value: Boolean

  def conjunction: BooleanConjunction =
    Newtype.pack[Boolean, BooleanConjunction](value)

  def |∧| : BooleanConjunction =
    conjunction

  def |/\| : BooleanConjunction =
    conjunction

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
  def ∧(q: => Boolean) =
    value && q

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
  def /\(q: => Boolean) =
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
  def ∨(q: => Boolean) =
    value || q

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
  def \/(q: => Boolean) =
    ∨(q)

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
  def !&&(q: => Boolean) =
    !value || !q

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
  def !||(q: => Boolean) =
    !value && !q

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
  def -->(q: => Boolean) =
    !value || q

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
  def <--(q: => Boolean) =
    value || !q

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
  def ⇏(q: => Boolean) =
    value && !q

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
  def -/>(q: => Boolean) =
    ⇏(q)

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
  def ⇍(q: => Boolean) =
    !value && q

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
  def <\-(q: => Boolean) =
    ⇍(q)

  /**
   * Executes the given side-effect if this boolean value is <code>false</code>.
   */
  def unless(f: => Unit) =
    if (!value) f

  /**
   * Executes the given side-effect if this boolean value is <code>true</code>.
   */
  def when(f: => Unit) =
    if (value) f

  /**
   * @return `a` if true, `b` otherwise
   */
  def fold[A](a: => A, b: => A): A = if (value) a else b

  trait Conditional[X] {
    def |(f: => X): X
  }

  /**
   * Conditional operator that returns the first argument if this is <code>true</code>, the second argument otherwise.
   */
  def ?[X](t: => X): Conditional[X] = new Conditional[X] {
    def |(f: => X) = if (value) t else f
  }

  /**
   * Returns the given argument in <code>Some</code> if this is <code>true</code>, <code>None</code> otherwise.
   */
  def option[A](a: => A): Option[A] =
    if (value) Some(a) else None

  /**
   * Returns the given argument in <code>lazySome</code> if this is <code>true</code>, <code>lazyNone</code> otherwise.
   */
  def lazyOption[A](a: => A): LazyOption[A] =
    if (value) LazyOption.lazySome(a) else LazyOption.lazyNone

  trait ConditionalEither[A] {
    def or[B](b: => B): Either[A, B]
  }

  /**
   * Returns the first argument in <code>Left</code> if this is <code>true</code>, otherwise the second argument in
   * <code>Right</code>.
   */
  def either[A, B](a: => A) = new ConditionalEither[A] {
    def or[B](b: => B) =
      if (value) Left(a) else Right(b)
  }

  trait ConditionalLazyEither[A] {
    def or[B](b: => B): LazyEither[A, B]
  }

  /**
   * Returns the first argument in <code>Left</code> if this is <code>true</code>, otherwise the second argument in
   * <code>Right</code>.
   */
  def lazyEither[A, B](a: => A) = new ConditionalLazyEither[A] {
    def or[B](b: => B) =
      if (value) lazyLeft(a) else lazyRight(b)
  }

  /**
   * Returns the given argument if this is <code>true</code>, otherwise, the zero element for the type of the given
   * argument.
   */
  def ??[A](a: => A)(implicit z: Zero[A]): A =
    if (value) a else z.zero

  /**
   * Returns the given argument if this is <code>false</code>, otherwise, the zero element for the type of the given
   * argument.
   */
  def !?[A](a: => A)(implicit z: Zero[A]): A =
    if (value) z.zero else a

  trait GuardPrevent[M[_]] {
    def apply[A](a: => A)(implicit p: PointedEmpty[M]): M[A]
  }

  def guard[M[_]] = new GuardPrevent[M] {
    def apply[A](a: => A)(implicit p: PointedEmpty[M]) =
      if (value) p.point(a) else p.e
  }

  def prevent[M[_]] = new GuardPrevent[M] {
    def apply[A](a: => A)(implicit p: PointedEmpty[M]) =
      if (value) p.e else p.point(a)
  }
}

object BooleanW extends BooleanWs

trait BooleanWs {
  implicit def BooleanTo(n: Boolean): BooleanW = new BooleanW {
    val value = n
  }
}