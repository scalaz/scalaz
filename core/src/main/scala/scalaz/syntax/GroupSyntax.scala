package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Group` */
trait GroupOps[F] extends Ops[F] {
  implicit def F: Group[F]
  ////
  final def |-|(other: => F): F = F.minus(self, other)
  final def inverse: F = F.inverse(self)
  final def unary_-  = inverse
  ////
}

trait ToGroupOps extends ToMonoidOps {
  implicit def ToGroupOps[F](v: F)(implicit F0: Group[F]) =
    new GroupOps[F] { def self = v; implicit def F: Group[F] = F0 }

  ////

  ////
}

trait GroupSyntax[F] extends MonoidSyntax[F] {
  implicit def ToGroupOps(v: F): GroupOps[F] = new GroupOps[F] { def self = v; implicit def F: Group[F] = GroupSyntax.this.F }
  
  def F: Group[F]
  ////

  ////
}
