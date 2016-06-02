package scalaz
package typeclass

trait StrongInstances { instances =>
  implicit val function: Strong[Function] = new Strong[Function] {
    override val profunctor = Profunctor.function

    override def first[A, B, C](pab: A => B): ((A, C)) => (B, C) = _ match {
      case (a, c) => (pab(a), c)
    }

    override def second[A, B, C](pab: A => B): ((C, A)) => (C, B) = _ match {
      case (c, a) => (c, pab(a))
    }
  }
}
