package scalaz


trait StateFunctions {
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

  /**
   * Computes the difference between the current and previous values of `a`
   */
  def delta[A](a: A)(implicit A: Group[A]): State[A, A] = State{
    (prevA) =>
      val diff = A.minus(a, prevA)
      (diff, a)
  }
}
