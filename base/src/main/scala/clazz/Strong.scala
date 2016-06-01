package scalaz
package clazz

trait Strong[P[_, _]] {
  def profunctor: Profunctor[P]

  def first[A, B, C](pab: P[A, B]): P[(A, C), (B, C)] =
    profunctor.dimap[(C, A), (C, B), (A, C), (B, C)](second(pab))(_.swap)(_.swap)

  def second[A, B, C](pab: P[A, B]): P[(C, A), (C, B)] =
    profunctor.dimap[(A, C), (B, C), (C, A), (C, B)](first(pab))(_.swap)(_.swap)
}

object Strong extends StrongInstances

