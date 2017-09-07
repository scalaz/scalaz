package scalaz
package data

import Prelude._
import Maybe.{Empty, Just}

trait MaybeFunctions {
  def empty[A]: Maybe[A] = Empty[A]
  def just[A](a: A): Maybe[A] = Just(a)

  def maybe[A, B](n: B)(f: A => B): Maybe[A] => B = _ match {
    case Just(x) => f(x)
    case _       => n
  }

  def fromOption[A](oa: Option[A]): Maybe[A] = oa.fold[Maybe[A]](empty[A])(Just(_))
}
