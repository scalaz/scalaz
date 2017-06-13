package scalaz
package typeclass

import data.Disjunction._

trait Choice[P[_, _]] {
  def profunctor: Profunctor[P]

  def leftchoice[A, B, C](pab: P[A, B]): P[A \/ C, B \/ C]

  def rightchoice[A, B, C](pab: P[A, B]): P[C \/ A, C \/ B]
}

object Choice extends ChoiceInstances {
  def apply[P[_, _]](implicit P: Choice[P]): Choice[P] = P
}

