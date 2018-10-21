package scalaz
package tc

import java.lang.String
import scala.language.experimental.macros

trait SemigroupClass[A] {
  def mappend(a1: A, a2: A): A
}

object SemigroupClass {
  implicit val StringSemigroup: Semigroup[String] = instanceOf[SemigroupClass[String]](_ + _)
}

trait SemigroupSyntax {
  implicit final class ToSemigroupOps[A](a: A) {
    def mappend(f: A)(implicit ev: Semigroup[A]): A = macro ops.Ops.ia_1
    type mappend
    def |+|(f: A)(implicit ev: Semigroup[A]): A = macro ops.Ops.nia_1[mappend]
  }
}
