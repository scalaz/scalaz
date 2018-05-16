package scalaz
package ct

import scala.language.experimental.macros

trait ProfunctorSyntax {
  implicit class ToProfunctorOps[F[_, _]: Profunctor, A, B](self: F[A, B]) {
    def lmap[C](f: C => A): F[C, B] = macro meta.Ops.f_1
    def rmap[C](f: B => C): F[A, C] = macro meta.Ops.f_1
    def dimap[C, D](f: C => A)(g: B => D): F[C, D] = macro meta.Ops.f_1_1
  }
}
