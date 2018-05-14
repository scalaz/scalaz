package scalaz
package typeclass

import scala.language.experimental.macros

trait FoldableSyntax {
  implicit final class ToFoldableOps[F[_], A](self: F[A]) {
    def foldLeft[B](f: B)(g: (B, A) => B)(implicit ev: Foldable[F]): B = macro meta.Ops.ia_1_1
    def foldRight[B](f: => B)(g: (A, => B) => B)(implicit ev: Foldable[F]): B = macro meta.Ops.ia_1_1
    def foldMap[B](f: A => B)(implicit g: Monoid[B], ev: Foldable[F]): B = macro meta.Ops.i_1_1i
    def toList(implicit ev: Foldable[F]): List[A] = macro meta.Ops.i_0
  }
}
