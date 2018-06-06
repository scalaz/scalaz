package scalaz
package ct

import scala.language.experimental.macros

@meta.minimal("dimap", ("lmap", "rmap"))
trait ProfunctorClass[F[_, _]] {
  // todo: consider having only two params lists, like Bifunctor
  def dimap[A, B, C, D](fab: F[A, B])(ca: C => A)(bd: B => D): F[C, D] =
    lmap(rmap(fab)(bd))(ca)

  def lmap[A, B, C](fab: F[A, B])(ca: C => A): F[C, B] = dimap(fab)(ca)(identity[B])
  def rmap[A, B, C](fab: F[A, B])(bc: B => C): F[A, C] = dimap(fab)(identity[A])(bc)
}

trait ProfunctorInstances {}

trait ProfunctorSyntax {
  implicit class ToProfunctorOps[F[_, _]: Profunctor, A, B](self: F[A, B]) {
    def lmap[C](f: C => A): F[C, B] = macro meta.Ops.f_1
    def rmap[C](f: B => C): F[A, C] = macro meta.Ops.f_1
    def dimap[C, D](f: C => A)(g: B => D): F[C, D] = macro meta.Ops.f_1_1
  }
}
