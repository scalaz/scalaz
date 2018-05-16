package scalaz
package ct

import data.Disjunction

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
