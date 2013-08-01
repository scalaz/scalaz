package scalaz
package syntax

trait NelOps[A] extends Ops[A] {
  final def wrapNel: NonEmptyList[A] =
    NonEmptyList(self)
}

trait ToNelOps {
  implicit def ToNelOps[A](a: A) = new NelOps[A]{ def self = a }
}
