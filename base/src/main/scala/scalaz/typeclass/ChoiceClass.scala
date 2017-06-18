package scalaz
package typeclass

import data.Disjunction._

trait ChoiceClass[P[_, _]] extends Choice[P] with ProfunctorClass[P] {
  final def choice: Choice[P] = this
}

object ChoiceClass {
  trait Left[P[_, _]] extends Alt[Left[P]] { self : Choice[P] =>
    override def leftchoice[A, B, C](pab: P[A, B]): P[A \/ C, B \/ C]
    override def rightchoice[A, B, C](pab: P[A, B]): P[C \/ A, C \/ B] =
      profunctor.dimap[A \/ C, B \/ C, C \/ A, C \/ B](leftchoice(pab))(swap(_))(swap(_))
  }

  trait Right[P[_, _]] extends Alt[Right[P]] { self : Choice[P] =>
    override def rightchoice[A, B, C](pab: P[A, B]): P[C \/ A, C \/ B]
    override def leftchoice[A, B, C](pab: P[A, B]): P[A \/ C, B \/ C] =
      profunctor.dimap[C \/ A, C \/ B, A \/ C, B \/ C](rightchoice(pab))(swap(_))(swap(_))
  }

  trait Alt[D <: Alt[D]] { self: D => }
}
