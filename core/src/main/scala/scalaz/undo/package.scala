package scalaz

package object undo {
  type StateTHistory[F[_], S, A] = StateT[F, History[S], A]

  type HStateTMonadState[F[_], S] = MonadState[({type f[s, a] = StateT[F, s, a]})#f, History[S]]
}
