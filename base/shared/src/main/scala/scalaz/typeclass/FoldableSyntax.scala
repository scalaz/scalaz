package scalaz
package typeclass

import com.github.ghik.silencer.silent

import scala.language.implicitConversions
import scala.language.experimental.macros

trait FoldableSyntax {
  implicit def foldableOps[F[_], A](fa: F[A])(implicit F: Foldable[F]): FoldableSyntax.Ops[F, A] =
    new FoldableSyntax.Ops(fa)
}

object FoldableSyntax {
  class Ops[F[_]: Foldable, A](@silent self: F[A]) {
    def foldLeft[B](f: B)(g: (B, A) => B): B = macro meta.Ops.fa_1_1
    def foldRight[B](f: => B)(g: (A, => B) => B): B = macro meta.Ops.fa_1_1
    def foldMap[B: Monoid](f: A => B): B = macro meta.Ops.f_1_1
    def toList: List[A] = macro meta.Ops.f_0
  }
}
