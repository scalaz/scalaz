package scalaz
package ct

import scala.language.experimental.macros

trait ProfunctorClass[F[_, _]] {
  def lmap[A, B, C](fab: F[A, B])(ca: C => A): F[C, B]
  def rmap[A, B, C](fab: F[A, B])(bc: B => C): F[A, C]
  def dimap[A, B, C, D](fab: F[A, B])(ca: C => A)(bd: B => D): F[C, D]
}

object ProfunctorClass {

  trait DeriveDimap[F[_, _]] extends ProfunctorClass[F] with Alt[DeriveDimap[F]] {
    final override def dimap[A, B, C, D](fab: F[A, B])(ca: C => A)(bd: B => D): F[C, D] = rmap(lmap(fab)(ca))(bd)
  }

  trait DeriveLRMap[F[_, _]] extends ProfunctorClass[F] with Alt[DeriveLRMap[F]] {
    final override def lmap[A, B, C](fab: F[A, B])(ca: C => A): F[C, B] = dimap[A, B, C, B](fab)(ca)(identity)
    final override def rmap[A, B, C](fab: F[A, B])(bc: B => C): F[A, C] = dimap[A, B, A, C](fab)(identity)(bc)
  }

  trait Alt[D <: Alt[D]]
}

trait ProfunctorFunctions {
  @inline final def dimap[F[_, _], A, B, C, D](fab: F[A, B])(
    ca: C => A,
    bd: B => D,
  )(implicit F: Profunctor[F]): F[C, D] =
    F.dimap(fab)(ca)(bd)

  /* TODO: these clash with the bifunctor versions. Decide on a name for them.
  @inline final def lmap[F[_, _], A, B, C](fab: F[A, B])(ca: C => A)(implicit F: Profunctor[F]): F[C, B] =
    F.lmap(fab)(ca)
  @inline final def rmap[F[_, _], A, B, D](fab: F[A, B])(bd: B => D)(implicit F: Profunctor[F]): F[A, D] =
    F.rmap(fab(bd)
 */
}

trait ProfunctorSyntax {
  implicit class ToProfunctorOps[F[_, _]: Profunctor, A, B](self: F[A, B]) {
    def lmap[C](f: C => A): F[C, B] = macro meta.Ops.f_1
    def rmap[C](f: B => C): F[A, C] = macro meta.Ops.f_1
    def dimap[C, D](f: C => A)(g: B => D): F[C, D] = macro meta.Ops.f_1_1
  }
}
