package scalaz
package tests

import scala.{ Int, List }

import data._, tc._, Scalaz._

import laws._
import laws.FunctorLaws.{Functor => FunctorLaws}

import testz.{Harness, Result, assert}

final class IListTests {
  val lists = List(
                IList.empty[Int],
                IList.cons(2, IList.empty),
                IList.cons(2, IList.cons(3, IList.empty))
              )

  def cross[A, B](l1: List[A], l2: List[B]): List[(A, B)] =
    l1.flatMap(a1 => l2.map(a2 => (a1, a2)))

  def assertEqual[A: Eq](a1: A, a2: A): Result =
    assert(a1 === a2)

  def tests[T](harness: Harness[T]): T = {
    import harness._
    section("laws")(
      section("eq laws")(
        test("reflexivity") { () =>
          lists.map(
            EqLaws.reflexivity(_)(assert)
          ).reduce(Result.combine(_, _))
        },
        test("identity") { () =>
          cross(lists, lists).map {
            case (l1, l2) =>
              EqLaws.identity(l1, l2)(_.foldRight(0)(_ + _))(
                (equal, leftSum, rightSum) =>
                  assert(equal == (leftSum == rightSum))
              )
          }.reduce(Result.combine(_, _))
        }
      ),
      section("monad laws")(
        test("functor identity") { () =>
          lists.map(
            FunctorLaws.identityToIdentity(_)(assertEqual[IList[Int]])
          ).reduce(Result.combine(_, _))
        },
        test("apply associativity") { () =>
          val listsAdd = lists.map(_.map(i => (n: Int) => i + n))
          val listsMult = lists.map(_.map(i => (n: Int) => i * n))

          cross(cross(lists, listsAdd), listsMult).map {
            case ((i, a), m) =>
              ApplyLaws.applyAssoc(i)(a, m)(
                assertEqual[IList[Int]]
              )
          }.reduce(Result.combine(_, _))
        }
      )
    )
  }
}
