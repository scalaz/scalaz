package scalaz
package typeclass

import com.github.ghik.silencer.silent

import scala.language.implicitConversions
import scala.language.experimental.macros

trait SemigroupSyntax {
  implicit def equalOpsA[A: Semigroup](a: A): SemigroupSyntax.OpsA[A] = new SemigroupSyntax.OpsA(a)
}

object SemigroupSyntax {
  class OpsA[A: Semigroup](@silent a: A) {
    def append(f: => A): A = macro meta.Ops._f1[A, A]
  }
}
