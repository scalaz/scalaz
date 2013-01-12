package scalaz


trait IndexedStateFunctions {
  def constantIndexedState[S1, S2, A](a: A, s: => S2): IndexedState[S1, S2, A] =
    IndexedState((_: S1) => (s, a))

  def iPut[S1, S2](s: S2): IndexedState[S1, S2, Unit] = IndexedState(_ => (s, ()))

  def iModify[S1, S2](f: S1 => S2): IndexedState[S1, S2, Unit] = IndexedState(s => {
    val r = f(s);
    (r, ())
  })
}

trait StateFunctions extends IndexedStateFunctions {
  def constantState[S, A](a: A, s: => S): State[S, A] =
    State((_: S) => (s, a))

  def state[S, A](a: A): State[S, A] =
    State((_ : S, a))

  def init[S]: State[S, S] = State(s => (s, s))

  def get[S]: State[S, S] = init

  def gets[S, T](f: S => T): State[S, T] = State(s => (s, f(s)))

  def put[S](s: S): State[S, Unit] = State(_ => (s, ()))

  def modify[S](f: S => S): State[S, Unit] = State(s => {
    val r = f(s);
    (r, ())
  })
}
