package scalaz
package syntax

final class NelOps[A](self: A) {
  final def wrapNel: NonEmptyList[A] =
    NonEmptyList(self)
}

trait ToNelOps {
  implicit def ToNelOps[A](a: A) = new NelOps(a)
}
