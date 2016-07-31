package scalaz
package typeclass

import data.Disjunction._

abstract class Profunctor[F[_, _]] {
  def lmap[A, B, C](fab: F[A, B])(ca: C => A): F[C, B] = dimap[A, B, C, B](fab)(ca)(identity)
  def rmap[A, B, C](fab: F[A, B])(bc: B => C): F[A, C] = dimap[A, B, A, C](fab)(identity)(bc)
  def dimap[A, B, C, D](fab: F[A, B])(ca: C => A)(bd: B => D): F[C, D] = rmap(lmap(fab)(ca))(bd)
}

object Profunctor extends ProfunctorInstances with ProfunctorSyntax {
  def apply[F[_, _]](implicit F: Profunctor[F]): Profunctor[F] = F
}
