package scalaz
package typeclass

import data.Disjunction._

trait Choice[P[_, _]] {
  def profunctor: Profunctor[P]

  def left[A, B, C](pab: P[A, B]): P[A \/ C, B \/ C] =
    profunctor.dimap[C \/ A, C \/ B, A \/ C, B \/ C](right(pab))(swap(_))(swap(_))

  def right[A, B, C](pab: P[A, B]): P[C \/ A, C \/ B] =
    profunctor.dimap[A \/ C, B \/ C, C \/ A, C \/ B](left(pab))(swap(_))(swap(_))
}

object Choice extends ChoiceInstances

