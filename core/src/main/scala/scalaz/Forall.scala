package scalaz

/** A universally quantified value */
trait Forall[P[_]] { self =>
  def apply[A]: P[A]

  /** `Forall` is an endofunctor in an endofunctor category */
  def map[Q[_]](f: P ~> Q) = new Forall[Q] {
    def apply[A]: Q[A] = f(self.apply)
  }
}

object Forall extends Foralls

trait Foralls {
  /** Universal quantification by doubly negating an existential. */
  type Not[A] = A => Nothing
  type DNE[P[_]] = Not[P[A]] forSome {type A}
  type CPS[P[_]] = Not[DNE[P]]

  /** Construct a universal quantifier by continuation-passing. */
  def apply[P[_]](p: CPS[P]): Forall[P] = new Forall[P] {
    def apply[A]: P[A] = {
      case class Control(arg: P[A]) extends Throwable(null, null, true, false)
      try {
        p((arg: P[A]) => throw new Control(arg))
      } catch {
        case Control(arg) => arg
      }
    }
  }
}
