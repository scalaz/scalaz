package scalaz
package tests

import scala.{ Int, List, Option }

import data._, Scalaz._

import laws._
import laws.FunctorLaws.{ Functor => FunctorLaws }

import testz.{ assert, Harness, Result }

import z._

final class IListTests {
  val lists = List(
    IList.empty[Int],
    IList.cons(20, IList.empty),
    IList.cons(20, IList.cons(30, IList.empty))
  )

  def cross[A, B](l1: List[A], l2: List[B]): List[(A, B)] =
    l1.flatMap(a1 => l2.map(a2 => (a1, a2)))

  def testAppend(append: (IList[Int], IList[Int]) => IList[Int]): Result =
    List(
      (append(IList(1, 2, 3), IList(4, 5, 6)), IList(1, 2, 3, 4, 5, 6)),
      (append(IList.empty, IList(4, 5, 6)), IList(4, 5, 6)),
      (append(IList(1, 2, 3), IList.empty), IList(1, 2, 3)),
    ).foldMap(assertEqualTupled)

  def tests[T](harness: Harness[T], sequence: (T, T) => T): T = {
    import harness._
    sequence(
      section("concrete")(
        test("append") { () =>
          testAppend(_.append(_))
        },
        test("++") { () =>
          testAppend(_ ++ _)
        },
        test(":::") { () =>
          testAppend(_ ::: _)
        },
        test("empty caching") { () =>
          val empty1 = IList.empty.asInstanceOf[scala.AnyRef]
          val empty2 = IList.empty.asInstanceOf[scala.AnyRef]
          assert(empty1 eq empty2)
        },
        test("apply") { () =>
          assertEqual(
            IList(1, 2, 3),
            IList.cons(1, IList.cons(2, IList.cons(3, IList.empty)))
          )
        },
        test("uncons") { () =>
          List(
            (IList.uncons(IList.cons(1, IList.empty)), Maybe2.just2(1, IList.empty[Int])),
            (IList.uncons(IList.empty[Int]), Maybe2.empty2[Int, IList[Int]]),
          ).foldMap(assertEqualTupled)
        },
        test("reverse") { () =>
          List(
            (IList.reverse(IList(1, 2, 3)), IList(3, 2, 1)),
            (IList.reverse(IList.empty[Int]), IList.empty[Int]),
          ).foldMap(assertEqualTupled)
        },
        test("unfoldRight") { () =>
          def keepLess5(i: Int): Maybe2[Int, Int] =
            if (i < 5) Maybe2.just2(i, i + 1)
            else Maybe2.empty2

          List(
            (IList.unfoldRight[Int, Int](_ => Maybe2.empty2)(0), IList.empty[Int]),
            (IList.unfoldRight[Int, Int](keepLess5)(0), IList(0, 1, 2, 3, 4)),
          ).foldMap(assertEqualTupled)
        },
        test("foldLeft") { () =>
          assertEqual(
            IList.foldLeft(IList(2, 3, 4), 2 * 3 * 4)(_ / _),
            1
          )
        },
        test("head") { () =>
          List(
            (IList.empty[Int].head, Maybe.empty[Int]),
            (IList(1).head, Maybe.just(1)),
            (IList(1, 2).head, Maybe.just(1)),
          ).foldMap(assertEqualTupled)
        },
        test("tail") { () =>
          List(
            (IList.empty[Int].tail, Maybe.empty[IList[Int]]),
            (IList(1).tail, Maybe.just(IList.empty[Int])),
            (IList(1, 2).tail, Maybe.just(IList(2))),
          ).foldMap(assertEqualTupled)
        },
        test("isEmpty") { () =>
          List(
            IList.empty[Int].isEmpty,
            !IList(1).isEmpty,
          ).foldMap(assert)
        },
        test("nonEmpty") { () =>
          List(
            !IList.empty[Int].nonEmpty,
            IList(1).nonEmpty,
          ).foldMap(assert)
        },
        test("reverse_:::") { () =>
          List(
            (IList.empty[Int] reverse_::: IList.empty, IList.empty[Int]),
            (IList(1, 2, 3) reverse_::: IList.empty, IList(3, 2, 1)),
            (IList.empty[Int] reverse_::: IList(1, 2, 3), IList(1, 2, 3)),
            (IList(1, 2, 3) reverse_::: IList(4, 5, 6), IList(3, 2, 1, 4, 5, 6)),
          ).foldMap(assertEqualTupled)
        },
        test("filter") { () =>
          List(
            (IList.empty[Int].filter(_ => false), IList.empty[Int]),
            (IList(1, 2, 3).filter(_ => true), IList(1, 2, 3)),
            (IList(1, 2, 3).filter(_ => false), IList.empty[Int]),
            (IList(1, 2, 3).filter(i => i % 2 != 0), IList(1, 3)),
          ).foldMap(assertEqualTupled)
        },
        test("exists") { () =>
          List(
            (!IList.empty[Int].exists(_ == 1)),
            (IList(1).exists(_ == 1)),
            (IList(1, 2).exists(_ == 2)),
          ).foldMap(assert)
        },
        test("forall") { () =>
          List(
            IList.empty[Int].forall(_ == 1),
            IList(1).forall(_ == 1),
            !IList(1, 2).forall(_ == 2),
          ).foldMap(assert)
        },
        test("find") { () =>
          List(
            (IList.empty[Int].find(_ == 1), Maybe.empty[Int]),
            (IList(1).find(_ == 1), Maybe.just(1)),
            (IList(1, 2, 3).find(_ > 1), Maybe.just(2)),
          ).foldMap(assertEqualTupled)
        },
        test("index") { () =>
          List(
            (IList.empty[Int].index(0), Maybe.empty[Int]),
            (IList(0, 1).index(0), Maybe.just(0)),
            (IList(0, 1).index(1), Maybe.just(1)),
          ).foldMap(assertEqualTupled)
        },
        test("zip") { () =>
          List(
            (IList.empty[Int].zip[Int](IList.empty), IList.empty[(Int, Int)]),
            (IList(1, 2).zip[Int](IList.empty), IList.empty[(Int, Int)]),
            (IList(1, 2).zip[Int](IList(1)), IList((1, 1))),
            (IList(1).zip[Int](IList(1, 2)), IList((1, 1))),
            (IList(1, 2).zip[Int](IList(1, 2)), IList((1, 1), (2, 2))),
          ).foldMap(assertEqualTupled)
        },
        test("zipWithIndex") { () =>
          List(
            (IList.empty[Int].zipWithIndex, IList.empty[(Int, Int)]),
            (IList(1, 2, 3).zipWithIndex, IList((0, 1), (1, 2), (2, 3))),
          ).foldMap(assertEqualTupled)
        },
        test("take") { () =>
          List(
            (IList.empty[Int].take(1), IList.empty[Int]),
            (IList(1).take(0), IList.empty[Int]),
            (IList(1, 2).take(2), IList(1, 2)),
            (IList(1, 2).take(3), IList(1, 2)),
          ).foldMap(assertEqualTupled)
        },
        test("drop") { () =>
          List(
            (IList.empty[Int].drop(1), IList.empty[Int]),
            (IList(1).drop(0), IList(1)),
            (IList(1, 2).drop(2), IList.empty[Int]),
            (IList(1, 2, 3).drop(1), IList(2, 3)),
          ).foldMap(assertEqualTupled)
        },
        test("takeWhile") { () =>
          List(
            (IList.empty[Int].takeWhile(_ => true), IList.empty[Int]),
            (IList.empty[Int].takeWhile(_ => false), IList.empty[Int]),
            (IList(1, 2).takeWhile(_ => true), IList(1, 2)),
            (IList(1, 2).takeWhile(_ => false), IList.empty[Int]),
            (IList(1, 2).takeWhile(_ === 1), IList(1)),
          ).foldMap(assertEqualTupled)
        },
        test("dropWhile") { () =>
          List(
            (IList.empty[Int].dropWhile(_ => true), IList.empty[Int]),
            (IList.empty[Int].dropWhile(_ => false), IList.empty[Int]),
            (IList(1, 2).dropWhile(_ => true), IList.empty[Int]),
            (IList(1, 2).dropWhile(_ => false), IList(1, 2)),
            (IList(1, 2).dropWhile(_ === 1), IList(2)),
          ).foldMap(assertEqualTupled)
        },
        test("size") { () =>
          List(
            (IList.empty.size, 0),
            (IList(1).size, 1),
            (IList(1, 2).size, 2),
          ).foldMap(assertEqualTupled)
        },
      ),
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
                EqLaws.identity(l1, l2)(_.foldLeft(10000)(_ / _))(
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
            val snd = (a: Int) => IList(a * 100, a * 200, a * 300)

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
              TraversableLaws.traverseComposition(_)(fst, snd)(
                assertEqual[Option[Option[IList[Int]]]]
              )
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
    )
  }
}
