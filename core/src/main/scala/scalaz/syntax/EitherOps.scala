package scalaz
package syntax

final class EitherOps[A](val self: A) extends AnyVal {
  final def left[B]: (A \/ B) =
    -\/(self)

  final def right[B]: (B \/ A) =
    \/-(self)
}

trait ToEitherOps {
  implicit def ToEitherOps[A](a: A): EitherOps[A] = new EitherOps(a)
}
