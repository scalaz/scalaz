package scalaz
package data

trait DisjunctionSyntax {
  implicit final class ToDisjunctionOps[A](a: A) {
    def left[B]: A \/ B = -\/(a)
    def right[B]: B \/ A = \/-(a)
  }

  implicit final class EitherAsDisjunction[A, B](ab: Either[A, B]) {
    def asDisjunction: A \/ B = Disjunction.fromEither(ab)
  }
}
