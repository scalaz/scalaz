package scalaz
package typeclass

import scala.language.implicitConversions
import scala.language.experimental.macros

trait ProfunctorSyntax {
  implicit def profunctorOps[F[_, _], A, B](fa: F[A, B])(implicit F: Profunctor[F]): ProfunctorSyntax.Ops[F, A, B] =
    new ProfunctorSyntax.Ops(fa)
}

object ProfunctorSyntax {

  //weird parameter names because of the macros 
  class Ops[F[_, _], A, B](self: F[A, B])(implicit F: Profunctor[F]) {
    def lmap[C](f: C => A): F[C, B] = macro meta.Ops._f1[C => A, F[C, B]]
    def rmap[C](f: B => C): F[A, C] = macro meta.Ops._f1[B => C, F[A, C]]
    def dimap[C, D](f: C => A)(g: B => D): F[C, D] = macro meta.Ops._f2[C => A, B => D, F[C, D]]
  }
}


