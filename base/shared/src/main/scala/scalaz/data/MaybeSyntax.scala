package scalaz
package data

trait MaybeSyntax {
  implicit final class OptionAsMaybe[A](oa: Option[A]) { def asMaybe: Maybe[A] = Maybe.fromOption(oa) }

  implicit final class ToMaybeOps[A](a: A) {
    def just: Maybe[A] = Maybe.just(a)
  }
}
