package scalaz
package ct

import data.Disjunction.swap

trait ChoiceClass[P[_, _]] extends ProfunctorClass[P] {

  def leftchoice[A, B, C](pab: P[A, B]): P[A \/ C, B \/ C]

  def rightchoice[A, B, C](pab: P[A, B]): P[C \/ A, C \/ B]
}

object ChoiceClass {

  trait DeriveRight[P[_, _]] extends ChoiceClass[P] with Alt[DeriveRight[P]] {
    override def rightchoice[A, B, C](pab: P[A, B]): P[C \/ A, C \/ B] =
      dimap[A \/ C, B \/ C, C \/ A, C \/ B](leftchoice(pab))(swap)(swap)
  }

  trait DeriveLeft[P[_, _]] extends ChoiceClass[P] with Alt[DeriveLeft[P]] {
    override def leftchoice[A, B, C](pab: P[A, B]): P[A \/ C, B \/ C] =
      dimap[C \/ A, C \/ B, A \/ C, B \/ C](rightchoice(pab))(swap)(swap)
  }

  trait Alt[D <: Alt[D]]
}
