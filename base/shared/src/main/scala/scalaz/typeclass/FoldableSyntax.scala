package scalaz
package typeclass

import scala.language.experimental.macros

trait FoldableSyntax {
  implicit final class ToFoldableOps[F[_]: Foldable, A](self: F[A]) {
    def foldLeft[B](f: B)(g: (B, A) => B): B = macro meta.Ops.fa_1_1
    def foldRight[B](f: => B)(g: (A, => B) => B): B = macro meta.Ops.fa_1_1
    def foldMap[B: Monoid](f: A => B): B = macro meta.Ops.f_1_1
    def toList: List[A] = macro meta.Ops.f_0
  }
}