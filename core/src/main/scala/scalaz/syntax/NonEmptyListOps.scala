package scalaz
package syntax

final class NelOps[A](val self: A) extends Super {
  final def wrapNel: NonEmptyList[A] =
    NonEmptyList(self)
}

trait ToNelOps {
  implicit def ToNelOps[A](a: A) = new NelOps(a)
}
