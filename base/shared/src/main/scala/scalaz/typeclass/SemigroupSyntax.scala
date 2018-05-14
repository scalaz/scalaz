package scalaz
package typeclass

import scala.language.experimental.macros

trait SemigroupSyntax {
  implicit final class ToSemigroupOps[A: Semigroup](a: A) {
    def append(f: => A): A = macro meta.Ops.fa_1

    def <>(f: => A): A = macro meta.Ops.fa_1
  }
}
