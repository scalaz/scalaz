package scalaz
package data

import typeclass.IsCovariant
import typeclass.Liskov._

import Maybe.{Empty, Just}

trait MaybeFunctions {
  def empty[A]: Maybe[A] = IsCovariant[Maybe].widen(Empty)
  def just[A](a: A): Maybe[A] = Just(a)

  def maybe[A, B](n: B)(f: A => B): Maybe[A] => B = _ match {
    case Empty    => n
    case Just(x)  => f(x)
  }

  def fromOption[A](oa: Option[A]): Maybe[A] = oa.fold[Maybe[A]](empty[A])(Just(_))
}
