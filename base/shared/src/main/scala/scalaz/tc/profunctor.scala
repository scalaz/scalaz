package scalaz
package tc

import Predef._

import scala.language.experimental.macros

@meta.minimal("dimap", ("lmap", "rmap"))
trait ProfunctorClass[F[_, _]] {
  def lmap[A, B, C](fab: F[A, B])(ca: C => A): F[C, B]                 = dimap[A, B, C, B](fab)(ca)(identity)
  def rmap[A, B, C](fab: F[A, B])(bc: B => C): F[A, C]                 = dimap[A, B, A, C](fab)(identity)(bc)
  def dimap[A, B, C, D](fab: F[A, B])(ca: C => A)(bd: B => D): F[C, D] = rmap(lmap(fab)(ca))(bd)
}

trait ProfunctorSyntax {
  implicit class ToProfunctorOps[F[_, _]: Profunctor, A, B](self: F[A, B]) {
    def lmap[C](f: C => A): F[C, B] = macro ops.Ops.f_1
    def dimap[C, D](f: C => A)(g: B => D): F[C, D] = macro ops.Ops.f_1_1
  }
}
