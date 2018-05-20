package scalaz
package ct

trait StrongClass[P[_, _]] extends ProfunctorClass[P] {
  def first[A, B, C](pab: P[A, B]): P[(A, C), (B, C)]
  def second[A, B, C](pab: P[A, B]): P[(C, A), (C, B)]
}

object StrongClass {

  trait DeriveSecond[P[_, _]] extends StrongClass[P] with Alt[DeriveSecond[P]] {
    final override def second[A, B, C](pab: P[A, B]): P[(C, A), (C, B)] =
      dimap[(A, C), (B, C), (C, A), (C, B)](first(pab))(_.swap)(_.swap)
  }

  trait DeriveFirst[P[_, _]] extends StrongClass[P] with Alt[DeriveFirst[P]] { self: Strong[P] =>
    final override def first[A, B, C](pab: P[A, B]): P[(A, C), (B, C)] =
      dimap[(C, A), (C, B), (A, C), (B, C)](second(pab))(_.swap)(_.swap)
  }

  trait Alt[D <: Alt[D]]
}
