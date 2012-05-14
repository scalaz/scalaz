package scalaz
package syntax

trait StateV[A] extends Ops[A] {
  def state[S]: State[S, A] = State.state(self)
  def stateT[F[+_]:Pointed, S]: StateT[F, S, A] = StateT.stateT[F, A, S](self)
}

trait ToStateOps {
  implicit def ToStateV[A](a: A) = new StateV[A]{ def self = a }
}
