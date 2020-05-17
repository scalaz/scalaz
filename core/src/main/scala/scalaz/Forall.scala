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
