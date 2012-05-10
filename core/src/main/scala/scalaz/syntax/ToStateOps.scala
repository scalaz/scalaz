package scalaz
package syntax

trait StateV[S, A] extends Ops[A] {
  def state: State[S, A] = State.state(self)
  def stateT[F[+_]:Pointed]: StateT[F, S, A] = StateT.stateT[F, A, S](self)
}

trait ToStateOps {
  implicit def ToStateV[S, A](a: A) = new StateV[S, A]{ def self = a }
}
