package scalaz
package syntax

final class TheseOps[A](self: A) {
  final def wrapThis[B]: A \&/ B =
    \&/.This(self)

  final def wrapThat[B]: B \&/ A =
    \&/.That(self)
}

trait ToTheseOps {
  implicit def ToTheseOps[A](a: A): TheseOps[A] = new TheseOps(a)
}
