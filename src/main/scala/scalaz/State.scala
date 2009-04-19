package scalaz

sealed trait State[S, +A] {
  def apply(s: S): (S, A)

  def map[B](f: A => B): State[S, B] = State.state(apply(_) match {
    case (s, a) => (s, f(a))
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State.state(apply(_) match {
    case (s, a) => f(a)(s)
  })
  
  def !(s: S) = apply(s)._2

  def ~>(s: S) = apply(s)._1

  def withs(f: S => S): State[S, A] = State.state(f andThen (apply(_)))
}

object State {
  def state[S, A](f: S => (S, A)) = new State[S, A] {
    def apply(s: S) = f(s)
  }

  def init[S] = state[S, S](s => (s, s))

  def modify[S](f: S => S) = init[S] flatMap (s => state(_ => (f(s), ())))
}
