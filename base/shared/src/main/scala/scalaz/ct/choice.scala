package scalaz
package ct

import scala.Function

import data.Disjunction

import data.Disjunction.swap

import scala.language.experimental.macros

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

trait ChoiceInstances {

  implicit val functionChoice: Choice[Function] = instanceOf(
    new ChoiceClass[Function] with ProfunctorClass.DeriveDimap[Function] {

      override def lmap[A, B, C](fab: Function[A, B])(ca: C => A): Function[C, B] =
        fab compose ca

      override def rmap[A, B, C](fab: Function[A, B])(bc: B => C): Function[A, C] =
        fab andThen bc

      override def leftchoice[A, B, C](ab: A => B): A \/ C => B \/ C =
        _.fold[B \/ C](a => Disjunction.left(ab(a)))(Disjunction.right)

      override def rightchoice[A, B, C](ab: A => B): C \/ A => C \/ B =
        _.fold[C \/ B](Disjunction.left)(a => Disjunction.right(ab(a)))
    }
  )
}

trait ChoiceSyntax {
  implicit final class ToChoiceOps[P[_, _], A, B](self: P[A, B]) {
    def leftchoice[C](implicit ev: Choice[P]): P[A \/ C, B \/ C] = macro meta.Ops.i_0
    def rightchoice[C](implicit ev: Choice[P]): P[C \/ A, C \/ B] = macro meta.Ops.i_0
  }
}
