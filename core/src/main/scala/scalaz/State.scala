package scalaz

object State extends StateFunctions with StateInstances {
  def apply[S, A](f: S => (A, S)): State[S, A] = new StateT[S, Id, A] {
    def apply(s: S) = f(s)
  }
}

trait StateInstances {
  import State._

  implicit def stateMonad[S]: MonadState[({type f[s, a] = State[s, a]})#f, S] =
    StateT.stateTMonadState[S, Id](Ident.id)
}

trait StateFunctions {
  type State[S, A] = StateT[S, Id, A]

  def constantState[S, A](a: A, s: => S): State[S, A] =
    State((_: S) => (a, s))

  def state[S, A](a: A): State[S, A] =
    State((a, _: S))

  def init[S]: State[S, S] = State(s => (s, s))

  def put[S](s: S): State[S, S] = State(_ => (s, s))

  def modify[S](f: S => S): State[S, S] = State(s => {
    val r = f(s);
    (r, r)
  })
}
