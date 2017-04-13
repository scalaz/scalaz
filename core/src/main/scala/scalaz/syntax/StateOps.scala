package scalaz
package syntax

final class StateOps[A](val self: A) extends AnyVal {
  def state[S]: State[S, A] = State.state[S, A](self)
  def stateT[F[_]:Monad, S]: StateT[F, S, A] = StateT.stateT[F, S, A](self)
}

trait ToStateOps {
  implicit def ToStateOps[A](a: A): StateOps[A] = new StateOps(a)
}
