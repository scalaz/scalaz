package scalaz
package typeclass

trait Profunctor[F[_, _]] {
  def lmap[A, B, C](fab: F[A, B])(ca: C => A): F[C, B]
  def rmap[A, B, C](fab: F[A, B])(bc: B => C): F[A, C]
  def dimap[A, B, C, D](fab: F[A, B])(ca: C => A)(bd: B => D): F[C, D]
}

object Profunctor extends ProfunctorInstances {

  trait LeftRightMap[F[_, _]] extends Alt[LeftRightMap[F]] { self: Profunctor[F] =>
    override def lmap[A, B, C](fab: F[A, B])(ca: C => A): F[C, B]
    override def rmap[A, B, C](fab: F[A, B])(bc: B => C): F[A, C]
    override def dimap[A, B, C, D](fab: F[A, B])(ca: C => A)(bd: B => D): F[C, D] = rmap(lmap(fab)(ca))(bd)
  }
  trait Dimap[F[_, _]] extends Alt[Dimap[F]] { self: Profunctor[F] =>
    override def dimap[A, B, C, D](fab: F[A, B])(ca: C => A)(bd: B => D): F[C, D]
    override def lmap[A, B, C](fab: F[A, B])(ca: C => A): F[C, B] = dimap[A, B, C, B](fab)(ca)(identity)
    override def rmap[A, B, C](fab: F[A, B])(bc: B => C): F[A, C] = dimap[A, B, A, C](fab)(identity)(bc)
  }
  trait Alt[D <: Alt[D]] { self: D => }

  def apply[F[_, _]](implicit F: Profunctor[F]): Profunctor[F] = F

  object syntax extends ProfunctorSyntax
}
