package scalaz
package clazz

import scala.language.implicitConversions

trait FoldableSyntax {
  implicit def foldableOps[F[_], A](fa: F[A])(implicit F: Foldable[F]): FoldableSyntax.Ops[F, A] =
    new FoldableSyntax.Ops(fa)
}

object FoldableSyntax {
  class Ops[F[_], A](self: F[A])(implicit F: Foldable[F]) {
    def foldLeft[B](z: B)(f: (B, A) => B): B = F.foldLeft(self, z)(f)
    def foldRight[B](z: => B)(f: (A, => B) => B): B = F.foldRight(self, z)(f)
    def toList: List[A] = F.toList(self)
  }
}
