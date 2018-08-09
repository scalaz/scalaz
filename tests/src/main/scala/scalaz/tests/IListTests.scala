package scalaz
package tests

import scala.{ Int, List, Option }

import data._, Scalaz._

import laws._
import laws.FunctorLaws.{ Functor => FunctorLaws }

import testz.{ assert, Harness }

import z._

final class IListTests {
  val lists = List(
    IList.empty[Int],
    IList.cons(20, IList.empty),
    IList.cons(20, IList.cons(30, IList.empty))
  )

  def cross[A, B](l1: List[A], l2: List[B]): List[(A, B)] =
    l1.flatMap(a1 => l2.map(a2 => (a1, a2)))

  def tests[T](harness: Harness[T]): T = {
    import harness._
    section("laws")(
      section("eq laws")(
        test("reflexivity") { () =>
          lists.foldMap(
            EqLaws.reflexivity(_)(assert)
          )
        },
        test("identity") { () =>
          cross(lists, lists).foldMap {
            case (l1, l2) =>
              EqLaws.identity(l1, l2)(_.foldRight(0)(_ + _))(
                (equal, leftSum, rightSum) => assertEqual(equal, (leftSum == rightSum))
              )
          }
        }
      ),
      section("monad laws")(
        test("functor identity") { () =>
          lists.foldMap(
            FunctorLaws.identityToIdentity(_)(assertEqual[IList[Int]])
          )
        },
        test("apply associativity") { () =>
          val listsAdd  = lists.map(_.map(i => (n: Int) => i + n))
          val listsMult = lists.map(_.map(i => (n: Int) => i * n))

          cross(cross(lists, listsAdd), listsMult).foldMap {
            case ((i, a), m) =>
              ApplyLaws.applyAssoc(i)(a, m)(
                assertEqual[IList[Int]]
              )
          }
        },
        test("applicative identity") { () =>
          lists.foldMap {
            ApplicativeLaws.applyIdentity(_)(assertEqual[IList[Int]])
          }
        },
        test("bind associativity") { () =>
          // non-overlapping
          val fst = (a: Int) => IList(a + 10, a + 20, a + 30)
          val snd = (a: Int) => IList(a + 100, a + 200, a + 300)

          lists.foldMap {
            BindLaws.bindAssoc(_)(fst, snd)(assertEqual[IList[Int]])
          }
        },
        test("monad identity") { () =>
          lists.foldMap {
            MonadLaws.bindIdentity(_)(assertEqual[IList[Int]])
          }
        }
      ),
      section("traversable laws")(
        test("traversable composition") { () =>
          val fst = (a: Int) => if (a % 20 == 0) scala.None else scala.Some(a % 20)
          val snd = (a: Int) => if (a % 5 == 0) scala.None else scala.Some(a  % 20)

          lists.foldMap {
            TraversableLaws.traverseComposition(_)(fst, snd)(assertEqual[Option[Option[IList[Int]]]])
          }
        },
        test("traversable identity") { () =>
          lists.foldMap {
            TraversableLaws.traverseIdentity(_)(assertEqual[IList[Int]])
          }
        }
      ),
      section("monoid laws")(
        test("mappend associativity") { () =>
          cross(cross(lists, lists), lists).foldMap {
            case ((l1, l2), l3) =>
              SemigroupLaws.assoc(l1, l2, l3)(assertEqual[IList[Int]])
          }
        },
        test("mappend left identity") { () =>
          lists.foldMap {
            MonoidLaws.leftIdentity(_)(assertEqual[IList[Int]])
          }
        },
        test("mappend right identity") { () =>
          lists.foldMap {
            MonoidLaws.rightIdentity(_)(assertEqual[IList[Int]])
          }
        }
      )
    )
  }
}
