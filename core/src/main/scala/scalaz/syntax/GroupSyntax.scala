package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Group` */
trait GroupV[F] extends SyntaxV[F] {
  implicit def F: Group[F]
  ////
  final def |-|(other: => F): F = F.minus(self, other)
  final def inverse: F = F.inverse(self)
  final def unary_-  = inverse
  ////
}

trait ToGroupV extends ToMonoidV {
  implicit def ToGroupV[F](v: F)(implicit F0: Group[F]) =
    new GroupV[F] { def self = v; implicit def F: Group[F] = F0 }

  ////

  ////
}

trait GroupSyntax[F] extends MonoidSyntax[F] {
  implicit def ToGroupV(v: F)(implicit F0: Group[F]): GroupV[F] = new GroupV[F] { def self = v; implicit def F: Group[F] = F0 }

  ////

  ////
}
