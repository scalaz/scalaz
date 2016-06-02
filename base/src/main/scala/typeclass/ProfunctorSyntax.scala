package scalaz
package typeclass

import scala.language.implicitConversions

trait ProfunctorSyntax {
  implicit def profunctorOps[F[_, _], A, B](fa: F[A, B])(implicit F: Profunctor[F]): ProfunctorSyntax.Ops[F, A, B] =
    new ProfunctorSyntax.Ops(fa)
}

object ProfunctorSyntax {
  class Ops[F[_, _], A, B](self: F[A, B])(implicit F: Profunctor[F]) {
    def lmap[C](ac: C => A): F[C, B] = F.lmap(self)(ac)
    def rmap[C](bc: B => C): F[A, C] = F.rmap(self)(bc)
    def dimap[C, D](ab: C => A)(bc: B => D): F[C, D] = F.dimap[A, B, C, D](self)(ab)(bc)
  }
}


