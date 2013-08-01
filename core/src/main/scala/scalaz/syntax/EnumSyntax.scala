package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Enum` */
sealed abstract class EnumOps[F] extends Ops[F] {
  implicit def F: Enum[F]
  ////
  final def succ: F =
    F succ self

  final def -+-(n: Int): F =
    F.succn(n, self)

  final def succx: Option[F] =
    F.succx.apply(self)

  final def pred: F =
    F pred self

  final def ---(n: Int): F =
    F.predn(n, self)

  final def predx: Option[F] =
    F.predx.apply(self)

  final def from: EphemeralStream[F] =
    F.from(self)

  final def fromStep(step: Int): EphemeralStream[F] =
    F.fromStep(step, self)

  final def |=>(to: F): EphemeralStream[F] =
    F.fromTo(self, to)

  final def |->(to: F): List[F] =
    F.fromToL(self, to)

  final def |==>(step: Int, to: F): EphemeralStream[F] =
    F.fromStepTo(step, self, to)

  final def |-->(step: Int, to: F): List[F] =
    F.fromStepToL(step, self, to)

  ////
}

trait ToEnumOps extends ToOrderOps {
  implicit def ToEnumOps[F](v: F)(implicit F0: Enum[F]) =
    new EnumOps[F] { def self = v; implicit def F: Enum[F] = F0 }

  ////

  ////
}

trait EnumSyntax[F] extends OrderSyntax[F] {
  implicit def ToEnumOps(v: F): EnumOps[F] = new EnumOps[F] { def self = v; implicit def F: Enum[F] = EnumSyntax.this.F }
  
  def F: Enum[F]
  ////

  ////
}
