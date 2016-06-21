package scalaz
package data

import Maybe.{Empty, Just}

trait MaybeFunctions {
  def empty[A]: Maybe[A] = Empty()
  def just[A](a: A): Maybe[A] = Just(a)

  def maybe[A, B](n: B)(f: A => B): Maybe[A] => B = _ match {
    case Empty() => n
    case Just(x) => f(x)
  }

  def fromOption[A](oa: Option[A]): Maybe[A] = oa.fold[Maybe[A]](Empty[A])(Just(_))
}
