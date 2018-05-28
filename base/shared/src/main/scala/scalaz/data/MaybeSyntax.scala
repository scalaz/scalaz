package scalaz
package data

import scala.Option

trait MaybeSyntax {
  implicit final class OptionAsMaybe[A](oa: Option[A]) { def asMaybe: Maybe[A] = Maybe.fromOption(oa) }

  implicit final class ToMaybeOps[A](a: A) {
    def just: Maybe[A] = Maybe.just(a)
  }
}
