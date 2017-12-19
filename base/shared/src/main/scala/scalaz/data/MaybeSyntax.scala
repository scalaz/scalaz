package scalaz
package data

trait MaybeSyntax extends MaybeFunctions {
  type Maybe[A] = data.Maybe[A]

  override def empty[A]                     = data.Maybe.empty[A]
  override def just[A](a: A)                = data.Maybe.just(a)
  override def maybe[A, B](n: B)(f: A => B) = data.Maybe.maybe(n)(f)
  override def fromOption[A](oa: Option[A]) = data.Maybe.fromOption(oa)
  override def toOption[A](ma: Maybe[A])    = data.Maybe.toOption(ma)

  implicit class OptionAsMaybe[A](oa: Option[A]) { def asMaybe: Maybe[A] = Maybe.fromOption(oa) }
}
