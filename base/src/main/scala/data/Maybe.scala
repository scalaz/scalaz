package scalaz
package data

sealed abstract class Maybe[A] {
  final def fold[B](f: A => B, b: => B): B = this match {
    case Maybe.Just(a)  => f(a)
    case _              => b
  }

  final def map[B](f: A => B): Maybe[B] =
    fold(f andThen Maybe.Just[B], Maybe.Empty[B])

  final def flatMap[B](f: A => Maybe[B]): Maybe[B] =
    fold(f, Maybe.Empty[B])
}

object Maybe extends MaybeFunctions with MaybeInstances with MaybeSyntax {
  final private[data] case object Empty extends Maybe[Nothing]
  final case class Just[A](a: A) extends Maybe[A]
}
