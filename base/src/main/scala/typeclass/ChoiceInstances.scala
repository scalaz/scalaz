package scalaz
package typeclass

import data.Disjunction
import data.Disjunction.\/

trait ChoiceInstances { instances =>
  implicit val function: Choice[Function] = new Choice[Function] {
    override val profunctor = Profunctor.function

    override def left[A, B, C](ab: A => B): A \/ C => B \/ C  =
      _.fold[B \/ C](a => Disjunction.left(ab(a)))(Disjunction.right(_))

    override def right[A, B, C](ab: A => B): C \/ A => C \/ B =
      _.fold[C \/ B](Disjunction.left(_))((a => Disjunction.right(ab(a))))
  }
}

