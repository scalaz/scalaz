package scalaz
package syntax

final class EitherOps[A](val self: A) extends Super {
  final def left[B]: (A \/ B) =
    \/.left(self)

  final def right[B]: (B \/ A) =
    \/.right(self)
}

trait ToEitherOps {
  implicit def ToEitherOps[A](a: A) = new EitherOps(a)
}
