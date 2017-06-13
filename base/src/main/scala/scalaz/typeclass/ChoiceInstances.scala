package scalaz
package typeclass

import data.Disjunction
import data.Disjunction.\/

trait ChoiceInstances { instances =>
  implicit val function: Choice[Function] = new ChoiceClass[Function] {
 
    override def dimap[A, B, C, D](fab: Function[A, B])(ca: C => A)(bd: B => D): Function[C, D] =
      profunctor.dimap(fab)(ca)(bd)
   
    override def lmap[A, B, C](fab: Function[A, B])(ca: C => A): Function[C, B] =
      profunctor.lmap(fab)(ca)
    
    override def rmap[A, B, C](fab: Function[A, B])(bc: B => C): Function[A, C] =
      profunctor.rmap(fab)(bc)

    override def leftchoice[A, B, C](ab: A => B): A \/ C => B \/ C  =
      _.fold[B \/ C](a => Disjunction.left(ab(a)))(Disjunction.right(_))

    override def rightchoice[A, B, C](ab: A => B): C \/ A => C \/ B =
      _.fold[C \/ B](Disjunction.left(_))((a => Disjunction.right(ab(a))))
  }
}

