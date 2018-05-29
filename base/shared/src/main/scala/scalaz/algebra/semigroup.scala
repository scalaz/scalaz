package scalaz
package algebra

import scala.language.experimental.macros

trait SemigroupClass[A] {
  def append(a1: A, a2: => A): A
}

trait SemigroupInstances {}

trait SemigroupSyntax {
  implicit final class ToSemigroupOps[A](a: A) {
    def append(f: => A)(implicit ev: Semigroup[A]): A = macro meta.Ops.ia_1
  }
}
