package scalaz
package typeclass

trait Profunctor[F[_, _]] {
  def lmap[A, B, C](fab: F[A, B])(ca: C => A): F[C, B]
  def rmap[A, B, C](fab: F[A, B])(bc: B => C): F[A, C]
  def dimap[A, B, C, D](fab: F[A, B])(ca: C => A)(bd: B => D): F[C, D]
}

object Profunctor extends ProfunctorInstances with ProfunctorSyntax {
  def apply[F[_, _]](implicit F: Profunctor[F]): Profunctor[F] = F
}
