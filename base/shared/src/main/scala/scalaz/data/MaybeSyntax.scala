package scalaz
package data

trait MaybeSyntax {
  implicit class OptionAsMaybe[A](oa: Option[A]) {
    final def asMaybe: Maybe[A] = Maybe.fromOption(oa)
  }
  implicit class MaybeOps[A](m: Maybe[A]) {
    final def fold[B](f: A => B, b: => B): B = Maybe.fold(m)(f, b)
  }
}
