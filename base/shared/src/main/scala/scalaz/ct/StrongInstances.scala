package scalaz
package ct

import scala.Function

trait StrongInstances { instances =>
  implicit val functionStrong: Strong[Function] = instanceOf(
    new StrongClass[Function] with ProfunctorClass.DeriveDimap[Function] {

      override def lmap[A, B, C](fab: A => B)(ca: C => A): C => B =
        fab compose ca

      override def rmap[A, B, C](fab: A => B)(bc: B => C): A => C =
        fab andThen bc

      override def first[A, B, C](pab: A => B): ((A, C)) => (B, C) = {
        case (a, c) => (pab(a), c)
      }

      override def second[A, B, C](pab: A => B): ((C, A)) => (C, B) = {
        case (c, a) => (c, pab(a))
      }
    }
  )
}
