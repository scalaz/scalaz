package scalaz
package data

sealed abstract class Maybe[A] {
  final def fold[B](f: A => B, b: => B): B = this match {
    case Maybe.Just(a)  => f(a)
    case _              => b
  }
}

object Maybe extends MaybeFunctions with MaybeInstances with MaybeSyntax {
  final private[data] class Empty[A] private[Maybe]() extends Maybe[A]
  private[data] val Empty = ∀.of[Empty].from(new Empty)
  final case class Just[A](a: A) extends Maybe[A]
}
