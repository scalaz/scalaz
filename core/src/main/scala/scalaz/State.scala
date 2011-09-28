package scalaz

trait States {
  type State[S, A] = StateT[S, Id, A]

  def apply[S, A](f: S => (A, S)): State[S, A] = new StateT[S, Id, A] {
    def apply(s: S) = f(s)
  }

  def init[S]: State[S, S] = State(s => (s, s))

  def put[S](s: S): State[S, S] = State(_ => (s, s))

  def modify[S](f: S => S): State[S, S] = State(s => {
    val r = f(s); (r, r)
  })

  implicit def state[S]: MonadState[({type f[s, a] = State[s, a]})#f, S] =
    StateT.stateTMonadState[S, Id](Ident.id)
}


object State extends States