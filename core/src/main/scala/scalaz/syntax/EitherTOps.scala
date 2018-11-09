package scalaz
package syntax

final class EitherTOps[V](private val self: V) extends AnyVal {
}

trait ToEitherTOps {
  implicit def ToEitherTOps[A](a: A): EitherTOps[A] = new EitherTOps(a)
}
