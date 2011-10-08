package scalaz

package object undo {
  type StateTHistory[S, F[_], A] = StateT[History[S], F, A]

  type HStateTMonadState[S, F[_]] = MonadState[({type f[s, a] = StateT[s, F, a]})#f, History[S]]
}
