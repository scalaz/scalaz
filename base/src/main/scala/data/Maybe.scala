package scalaz
package data

sealed abstract class Maybe[A] {
  final def fold[B](f: A => B, b: => B): B = this match {
    case Maybe.Just(a)  => f(a)
    case Maybe.Empty()  => b
  }
}

object Maybe extends MaybeFunctions with MaybeInstances with MaybeSyntax {
  final case class Empty[A]() extends Maybe[A]
  final case class Just[A](a: A) extends Maybe[A]

  object Optics extends MaybeOptics
}
