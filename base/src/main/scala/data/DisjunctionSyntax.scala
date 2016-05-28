package scalaz
package data

import Disjunction.{\/, \/-, -\/}

trait DisjunctionSyntax {
  implicit class ToEitherOps[A](a: A) {
    def left[B]: A \/ B = -\/(a)
    def right[B]: B \/ A = \/-(a)
  }

  implicit class EitherAsDisjunction[A, B](ab: Either[A, B]) {
    def asDisjunction: A \/ B = Disjunction.fromEither(ab)
  }
}
