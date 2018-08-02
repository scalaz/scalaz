package scalaz
package tc

import java.lang.String
import scala.language.experimental.macros

trait SemigroupClass[A] {
  def mappend(a1: A, a2: => A): A
}

object SemigroupClass {
  implicit val StringSemigroup: Semigroup[String] = instanceOf(new SemigroupClass[String] {
    def mappend(a1: String, a2: => String) = a1 + a2
  })
}

trait SemigroupSyntax {
  implicit final class ToSemigroupOps[A](a: A) {
    def mappend(f: => A)(implicit ev: Semigroup[A]): A = macro meta.Ops.ia_1
  }
}
