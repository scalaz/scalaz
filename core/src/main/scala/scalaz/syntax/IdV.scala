package scalaz.syntax

import annotation.tailrec
import scalaz.{Pointed, Monoid, NonEmptyList, Enum, EphemeralStream, Equal}


trait IdV[A] extends SyntaxV[A] {
  /**Returns `self` if it is non-null, otherwise returns `d`. */
  final def ??(d: => A)(implicit ev: Null <:< A): A =
    if (self == null) d else self

  /**Applies `self` to the provided function */
  final def |>[B](f: A => B): B =
    f(self)

  final def squared: (A, A) =
    (self, self)

  def left[B]: Either[A, B] =
    Left(self)

  def right[B]: Either[B, A] =
    Right(self)

  final def wrapNel: NonEmptyList[A] =
    NonEmptyList(self)

  /**
   * @return the result of pf(value) if defined, otherwise the the Zero element of type B.
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
   * otherwise lift `self` into `F` with the provided [[scalaz.Pointed]].
   */
  def visit[F[_] : Pointed](p: PartialFunction[A, F[A]]): F[A] =
    if (p isDefinedAt self) p(self)
    else Pointed[F].point(self)

  def succ(implicit e: Enum[A]): A =
    e succ self

  def -+-(n: Int)(implicit e: Enum[A]): A =
    e.succn(n)(self)

  def succx(implicit e: Enum[A]): Option[A] =
    e.succx.apply(self)

  def pred(implicit e: Enum[A]): A =
    e pred self

  def ---(n: Int)(implicit e: Enum[A]): A =
    e.predn(n)(self)

  def predx(implicit e: Enum[A]): Option[A] =
    e.predx.apply(self)

  def from(implicit e: Enum[A]): EphemeralStream[A] =
    e.from(self)

  def fromStep(step: Int)(implicit e: Enum[A]): EphemeralStream[A] =
    e.fromStep(step, self)

  def |=>(to: A)(implicit e: Enum[A]): EphemeralStream[A] =
    e.fromTo(self, to)

  def |==>(step: Int, to: A)(implicit e: Enum[A]): EphemeralStream[A] =
    e.fromStepTo(step, self, to)
}

trait ToIdV {
  implicit def ToIdV[A](a: A): IdV[A] = new IdV[A] {
    def self: A = a
  }
}