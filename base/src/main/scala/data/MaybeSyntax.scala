package scalaz
package data

trait MaybeSyntax {
  implicit class OptionAsMaybe[A](oa: Option[A]) { def asMaybe: Maybe[A] = Maybe.fromOption(oa) }
}
