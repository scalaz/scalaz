package scalaz

/**
 * Stateful computation.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait State[S, +A] {
  /**
   * Perform the computation and pass the state through.
   */
  def apply(s: S): (S, A)

  /**
   * Applies the given value and returns the computed value.
   */
  def !(s: S) = apply(s)._2

  /**
   * Applies the given value and returns the state value.
   */
  def ~>(s: S) = apply(s)._1

  /**
   * Runs the given function through this state.
   */
  def state[B](f: ((S, A)) => (S, B)) = State.state((apply(_)) andThen f)

  /**
   * Apply the given function to this state and return the computed state.
   */
  def withs(f: S => S): State[S, A] = State.state(f andThen (apply(_)))

  /**
   * Maps the given function across a state.
   */
  def map[B](f: A => B): State[S, B] = State.state(apply(_) match {
    case (s, a) => (s, f(a))
  })

  /**
   * Binds the given function across a state.
   */
  def flatMap[B](f: A => State[S, B]): State[S, B] = State.state(apply(_) match {
    case (s, a) => f(a)(s)
  })
}

/**
 * Functions over state.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object State {
  /**
   * Constructs a state from the given state-passing function.
   */
  def state[S, A](f: S => (S, A)) = new State[S, A] {
    def apply(s: S) = f(s)
  }

  /**
   * Constructs a state from the given value.
   */
  def value[S] = new {
    def apply[A](a: A) = state((_: S, a))
  }

  /**
   * An initial state with the state itself in the computed value.
   */
  def init[S] = state[S, S](s => (s, s))

  /**
   * Construct a state that always returns the given value.
   */
  def constant[S, A](s: S, a: A) = state((x: S) => (s, a))

  /**
   * Modify the state with the given function.
   */
  def modify[S](f: S => S) = init[S] flatMap (s => state(x => (f(s), ())))
}
