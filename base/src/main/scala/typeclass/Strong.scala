package scalaz
package typeclass

trait Strong[P[_, _]] {
  def profunctor: Profunctor[P]

  def first[A, B, C](pab: P[A, B]): P[(A, C), (B, C)]

  def second[A, B, C](pab: P[A, B]): P[(C, A), (C, B)]
}

object Strong extends StrongInstances {
  trait First[P[_, _]] extends Alt[First[P]]{ self: Strong[P] =>
    def first[A, B, C](pab: P[A, B]): P[(A, C), (B, C)] =
      profunctor.dimap[(C, A), (C, B), (A, C), (B, C)](second(pab))(_.swap)(_.swap)
  }
  trait Second[P[_, _]] extends Alt[Second[P]]{ self: Strong[P] =>
    def second[A, B, C](pab: P[A, B]): P[(C, A), (C, B)] =
      profunctor.dimap[(A, C), (B, C), (C, A), (C, B)](first(pab))(_.swap)(_.swap)
  }
  trait Alt[D <: Alt[D]] { self: D => }
}

