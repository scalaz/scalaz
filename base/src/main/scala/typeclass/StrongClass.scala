package scalaz
package typeclass

trait StrongClass[P[_, _]] extends Strong[P] with ProfunctorClass[P] {
  final def strong: Strong[P] = this
}

object StrongClass {
  trait First[P[_, _]] extends Alt[First[P]]{ self: Strong[P] =>
    override def first[A, B, C](pab: P[A, B]): P[(A, C), (B, C)]
    override def second[A, B, C](pab: P[A, B]): P[(C, A), (C, B)] =
      profunctor.dimap[(A, C), (B, C), (C, A), (C, B)](first(pab))(_.swap)(_.swap)
  }

  trait Second[P[_, _]] extends Alt[Second[P]]{ self: Strong[P] =>
    override def second[A, B, C](pab: P[A, B]): P[(C, A), (C, B)]
    override def first[A, B, C](pab: P[A, B]): P[(A, C), (B, C)] =
      profunctor.dimap[(C, A), (C, B), (A, C), (B, C)](second(pab))(_.swap)(_.swap)
  }

  trait Alt[D <: Alt[D]] { self: D => }
}
