package scalaz
package data

import Prelude._

import As.AsOps

import Maybe.{Empty, Just}

trait MaybeFunctions {
  def empty[A]: Maybe[A] = As.reify[Nothing, A].substCoF[Maybe](Empty)
  def just[A](a: A): Maybe[A] = Just(a)

  def maybe[A, B](n: B)(f: A => B): Maybe[A] => B = _ match {
    case Empty    => n
    case Just(x)  => f(x)
  }

  def fromOption[A](oa: Option[A]): Maybe[A] = oa.fold[Maybe[A]](empty[A])(Just(_))
}
