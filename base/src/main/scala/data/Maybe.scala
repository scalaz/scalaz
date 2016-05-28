package scalaz
package data

sealed abstract class Maybe[A] {
  final def fold[B](f: A => B, b: => B): B = this match {
    case Maybe.Just(a)  => f(a)
    case Maybe.Empty()  => b
  }
}

object Maybe extends MaybeInstances with MaybeSyntax {
  final case class Empty[A]() extends Maybe[A]
  final case class Just[A](a: A) extends Maybe[A]

  def empty[A]: Maybe[A] = Empty()
  def just[A](a: A): Maybe[A] = Just(a)

  def maybe[A, B](n: B)(f: A => B): Maybe[A] => B = _ match {
    case Empty() => n
    case Just(x) => f(x)
  }

  def fromOption[A](oa: Option[A]): Maybe[A] = oa.fold[Maybe[A]](Empty[A])(Just(_))
}



