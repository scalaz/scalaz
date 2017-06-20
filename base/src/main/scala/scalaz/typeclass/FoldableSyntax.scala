package scalaz
package typeclass

import scala.language.implicitConversions
import scala.language.experimental.macros

trait FoldableSyntax {
  implicit def foldableOps[F[_], A](fa: F[A])(implicit F: Foldable[F]): FoldableSyntax.Ops[F, A] =
    new FoldableSyntax.Ops(fa)
}

object FoldableSyntax {
  class Ops[F[_], A](self: F[A])(implicit F: Foldable[F]) {
    def foldLeft[B](f: B)(g: (B, A) => B): B = F.foldLeft(self, f)(g)
    def foldRight[B](f: => B)(g: (A, => B) => B): B = F.foldRight(self, f)(g) //TODO: macro-ize foldable syntax
    def toList: List[A] = macro meta.Ops._f0[List[A]]
  }
}
