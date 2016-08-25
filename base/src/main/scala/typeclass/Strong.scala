package scalaz
package typeclass

trait Strong[P[_, _]] {
  def profunctor: Profunctor[P]

  def first[A, B, C](pab: P[A, B]): P[(A, C), (B, C)]

  def second[A, B, C](pab: P[A, B]): P[(C, A), (C, B)]
}

object Strong extends StrongInstances {
  def apply[F[_, _]](implicit F: Strong[F]): Strong[F] = F
}

