package scalaz
package syntax

final class StateOps[A](private val self: A) extends AnyVal {
  def state[S]: State[S, A] = State.state[S, A](self)
  def stateT[F[_]: Applicative, S]: StateT[S, F, A] = StateT.stateT[S, F, A](self)
}

trait ToStateOps {
  implicit def ToStateOps[A](a: A): StateOps[A] = new StateOps(a)
}
