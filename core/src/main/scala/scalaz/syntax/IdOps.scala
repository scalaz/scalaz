package scalaz.syntax

import annotation.tailrec
import scalaz.{Pointed, Monoid, NonEmptyList, Kleisli, Id, Reader}


trait IdOps[A] extends Ops[A] {
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

  def liftKleisli[R]: Kleisli[Id, R, A] =
    Kleisli[Id, R, A](_ => self)

  def liftReader[R]: Reader[R, A] =
    liftKleisli
}

trait ToIdOps {
  implicit def ToIdOps[A](a: A): IdOps[A] = new IdOps[A] {
    def self: A = a
  }
}
