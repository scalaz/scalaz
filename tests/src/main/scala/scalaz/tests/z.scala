package scalaz
package tests

import tc._

import testz._

object z {
  implicit val resultMonoid: Monoid[Result] =
    instanceOf(new MonoidClass[Result] {
      def mempty: Result = Succeed
      def mappend(a1: Result, a2: => Result): Result = Result.combine(a1, a2)
    })
}
