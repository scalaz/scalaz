package scalaz.typelevel.syntax

import scalaz.typelevel.{Succ, Nat}

final class NatOps[N <: Nat](nat: N) {
  def succ: Succ[N] = Succ(nat)
}

trait Nats {
  implicit def ToNatOps[N <: Nat](nat: N): NatOps[N] =
    new NatOps(nat)
}
