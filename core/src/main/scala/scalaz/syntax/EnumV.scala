package scalaz
package syntax

trait EnumV[F] extends SyntaxV[F] {
  implicit def F: Enum[F]
  
  final def succ: F =
    F succ self

  final def -+-(n: Int): F =
    F.succn(n)(self)

  final def succx: Option[F] =
    F.succx.apply(self)

  final def pred: F =
    F pred self

  final def ---(n: Int): F =
    F.predn(n)(self)

  final def predx: Option[F] =
    F.predx.apply(self)

  final def from: EphemeralStream[F] =
    F.from(self)

  final def fromStep(step: Int): EphemeralStream[F] =
    F.fromStep(step, self)

  final def |=>(to: F): EphemeralStream[F] =
    F.fromTo(self, to)

  final def |==>(step: Int, to: F): EphemeralStream[F] =
    F.fromStepTo(step, self, to)
}
