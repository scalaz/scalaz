package scalaz
package typeclass

import data.Disjunction._

trait Choice[P[_, _]] {
  def profunctor: Profunctor[P]

  def left[A, B, C](pab: P[A, B]): P[A \/ C, B \/ C]

  def right[A, B, C](pab: P[A, B]): P[C \/ A, C \/ B]
}

object Choice extends ChoiceInstances {

  trait Left[P[_, _]] extends Alt[Left[P]] { self : Choice[P] =>
    override def left[A, B, C](pab: P[A, B]): P[A \/ C, B \/ C]
    override def right[A, B, C](pab: P[A, B]): P[C \/ A, C \/ B] =
      profunctor.dimap[A \/ C, B \/ C, C \/ A, C \/ B](left(pab))(swap(_))(swap(_))
  }
  trait Right[P[_, _]] extends Alt[Right[P]] { self : Choice[P] =>
    override def right[A, B, C](pab: P[A, B]): P[C \/ A, C \/ B]
    override def left[A, B, C](pab: P[A, B]): P[A \/ C, B \/ C] =
      profunctor.dimap[C \/ A, C \/ B, A \/ C, B \/ C](right(pab))(swap(_))(swap(_))
  }
  trait Alt[D <: Alt[D]] { self: D => }

  def apply[P[_, _]](implicit P: Choice[P]): Choice[P] = P

}

