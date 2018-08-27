package scalaz
package tc

import scala.Int
import java.lang.String

import scala.annotation.tailrec
import scala.language.experimental.macros

trait SemigroupClass[A] {
  def mappend(a1: A, a2: => A): A

  def exponent(a: A, i: Int): A = {
    @tailrec
    def go(a0: A, j: Int): A =
      if (j == 0) a0 else go(mappend(a, a), j - 1)
    go(a, i)
  }
}

object SemigroupClass {
  implicit val StringSemigroup: Semigroup[String] = instanceOf(new SemigroupClass[String] {
    def mappend(a1: String, a2: => String) = a1 + a2
  })
}

trait SemigroupSyntax {
  implicit final class ToSemigroupOps[A](a: A) {
    def mappend(f: => A)(implicit ev: Semigroup[A]): A = macro meta.Ops.ia_1
    type mappend
    def |+|(f: => A)(implicit ev: Semigroup[A]): A = macro meta.Ops.nia_1[mappend]
  }
}
