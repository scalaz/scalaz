package scalaz
package syntax

final class MaybeOps[A](self: A) {
  final def just: Maybe[A] = Maybe.just(self)
}

trait ToMaybeOps {
  implicit def ToMaybeOps[A](a: A) = new MaybeOps(a)
}
