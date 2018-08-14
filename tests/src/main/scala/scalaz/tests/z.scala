package scalaz
package tests

import scala.Predef.Map

import tc._, Scalaz._

import testz._

object z {
  implicit val resultMonoid: Monoid[Result] =
    instanceOf(new MonoidClass[Result] {
      def mempty: Result                             = Succeed
      def mappend(a1: Result, a2: => Result): Result = Result.combine(a1, a2)
    })

  def assertEqual[A: Eq](a1: A, a2: A): Result =
    assert(a1 === a2)

  def assertEqualTupled[A: Eq]: ((A, A)) => Result =
    t => assertEqual(t._1, t._2)

  def assertEqualMaps[K, V](fst: Map[K, V], snd: Map[K, V]): Result =
    assert(fst == snd)

  def assertEqualNonEmptyMaps[K, V](fst: Map[K, V], snd: Map[K, V]): Result =
    assert(fst.nonEmpty) |+| assertEqualMaps(fst, snd)

}
