package scalaz
package syntax

trait EitherOps[A] extends Ops[A] {
  final def left[B]: (A \/ B) =
    \/.left(self)

  final def right[B]: (B \/ A) =
    \/.right(self)
}

trait ToEitherOps {
  implicit def ToEitherOps[A](a: A) = new EitherOps[A]{ def self = a }
}
