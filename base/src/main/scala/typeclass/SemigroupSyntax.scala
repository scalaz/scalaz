package scalaz
package typeclass

import scala.language.implicitConversions

trait SemigroupSyntax {
  implicit def equalOpsA[A: Semigroup](a: A): SemigroupSyntax.OpsA[A] = new SemigroupSyntax.OpsA(a)
}

object SemigroupSyntax {
  class OpsA[A](a: A)(implicit A: Semigroup[A]) {
    def append(other: => A): A = A.append(a, other)
  }
}
