package scalaz
package typeclass

import scala.language.experimental.macros

trait SemigroupSyntax {
  implicit final class ToSemigroupOps[A](a: A) {
    def append(f: => A)(implicit ev: Semigroup[A]): A = macro meta.Ops.ia_1
  }
}
