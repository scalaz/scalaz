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

  trait StateValue[S] {
    def apply[A](a: A): State[S, A]
  }

  def value[S] = new StateValue[S] {
    def apply[A](a: A) = state((_: S, a))
  }

  def init[S] = state[S, S](s => (s, s))

  def constant[S, A](s: S, a: A) = state((x: S) => (s, a))

  def modify[S](f: S => S) = init[S] flatMap (s => state(x => (f(s), ())))
}
