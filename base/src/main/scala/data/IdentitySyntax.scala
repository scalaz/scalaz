package scalaz
package data

import annotation.tailrec
import scalaz.typeclass.{Applicative, Monoid}
import language.implicitConversions

final class IdOps[A](val self: A) extends AnyVal {
  /** Applies the provided function to `self`. The Thrush combinator. */
  def |>[B](f: A => B): B =
    f(self)

  /** Tuples `self` with `self`. The diagonal of `A`. */
  def diagonal: (A, A) =
    (self, self)

  /** Returns `Unit`. */
  def ignore: Unit = ()

  /**
    * @return the result of pf(self) if defined, otherwise the the empty element of `B`.
    */
  def applyOrEmpty[B: Monoid](pf: PartialFunction[A, B]): B =
    pf.lift(self) match {
      case None    => Monoid[B].empty
      case Some(x) => x
    }

  /** Repeatedly apply `f`, seeded with `self`, until a `B` is returned. */
  def unfold[B](f: A => A Either B): B = {
    @tailrec
    def loop(value: A): B = f(value) match {
      case Right(r) => r
      case Left(l) => loop(l)
    }
    loop(self)
  }
}

trait ToIdOps {
  implicit def ToIdOps[A](a: A): IdOps[A] = new IdOps(a)
}
