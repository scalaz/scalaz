package scalaz
package data

trait MaybeSyntax {
  def empty[A]                     = data.Maybe.empty[A]
  def just[A](a: A)                = data.Maybe.just(a)
  def maybe[A, B](n: B)(f: A => B) = data.Maybe.maybe(n)(f)
  def fromOption[A](oa: Option[A]) = data.Maybe.fromOption(oa)
  def toOption[A](ma: Maybe[A])    = data.Maybe.toOption(ma)

  implicit final class OptionAsMaybe[A](oa: Option[A]) { def asMaybe: Maybe[A] = Maybe.fromOption(oa) }
}
