package scalaz
package syntax

final class NelOps[A](private val self: A) extends AnyVal {
  final def wrapNel: NonEmptyList[A] =
    NonEmptyList(self)
}

trait ToNelOps {
  implicit def ToNelOps[A](a: A): NelOps[A] = new NelOps(a)
}
