package scalaz
package algebra

import scala.language.experimental.macros

trait SemigroupClass[A] {
  def mappend(a1: A, a2: => A): A
}

trait SemigroupSyntax {
  implicit final class ToSemigroupOps[A](a: A) {
    def mappend(f: => A)(implicit ev: Semigroup[A]): A = macro meta.Ops.ia_1
  }
}
