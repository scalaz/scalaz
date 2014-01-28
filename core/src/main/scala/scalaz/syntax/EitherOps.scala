package scalaz
package syntax

final class EitherOps[A](self: A) {
  final def left[B]: (A \/ B) =
    -\/(self)

  final def right[B]: (B \/ A) =
    \/-(self)
}

trait ToEitherOps {
  implicit def ToEitherOps[A](a: A) = new EitherOps(a)
}
