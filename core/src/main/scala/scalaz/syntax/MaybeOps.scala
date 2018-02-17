package scalaz
package syntax

final class MaybeOps[A](private val self: A) extends AnyVal {
  final def just: Maybe[A] = Maybe.just(self)
}

trait ToMaybeOps {
  implicit def ToMaybeOps[A](a: A): MaybeOps[A] = new MaybeOps(a)
}
