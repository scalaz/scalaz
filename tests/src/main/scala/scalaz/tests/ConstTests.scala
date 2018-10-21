package scalaz
package tests

import scala.{ Boolean, Double, Int }

import data._, Scalaz._

import laws._

import testz.{ assert, Harness }

import z._

object ConstTests {

  val consts = IList(
    Const[IList[Int], IList[Double]](IList(1)),
    Const[IList[Int], IList[Double]](IList())
  )

  def tests[T](harness: Harness[T]): T = {
    import harness._
    section(
      namedSection("functions")(
        test("run . apply") { () =>
          val testValue: Int = 1234
          assertEqual(Const.run(Const[Int, IList[Int]](testValue)), testValue)
        },
        test("apply . run") { () =>
          val testValue: Const[Int, IList[Int]] =
            Const[Int, IList[Int]](2134)
          assertEqual(Const[Int, IList[Int]](Const.run(testValue)), testValue)
        }
      ),
      namedSection("instances")(
        namedSection("eq")(
          test("reflexivity") { () =>
            consts.foldMapStrict(
              EqLaws.reflexivity(_)(assert)
            )
          },
          test("identity") { () =>
            consts.cross(consts).foldMapStrict {
              case (c1, c2) =>
                assert((c1 === c2) === (Const.run(c1) === Const.run(c2)))
            }
          }
        ),
        namedSection("applicative")(
          test("apply associativity") { () =>
            consts.foldMapStrict(
              ApplyLaws.applyAssoc(_)(
                Const[IList[Int], IList[Double] => IList[Boolean]](IList(0)),
                Const[IList[Int], IList[Boolean] => Int](IList(1))
              )(assertEqual[Const[IList[Int], Int]](_, _))
            )
          },
          test("applicative identity") { () =>
            consts.foldMapStrict {
              ApplicativeLaws.applyIdentity(_)(assertEqual[Const[IList[Int], IList[Double]]])
            }
          }
        ),
        namedSection("traversable")(
          test("traversable composition") { () =>
            val fst = (_: IList[Double]).head
            val snd = (a: Double) => if (a > 0.0) Maybe.just(a) else Maybe.empty[Double]

            consts.foldMapStrict {
              TraversableLaws.traverseComposition(_)(fst, snd)(assertEqual[Maybe[Maybe[Const[IList[Int], Double]]]])
            }
          },
          test("traversable identity") { () =>
            consts.foldMapStrict {
              TraversableLaws.traverseIdentity(_)(assertEqual[Const[IList[Int], IList[Double]]])
            }
          }
        ),
        namedSection("monoid")(
          test("mappend associativity") { () =>
            consts.cross(consts).cross(consts).foldMapStrict {
              case ((l1, l2), l3) =>
                SemigroupLaws.assoc(l1, l2, l3)(assertEqual[Const[IList[Int], IList[Double]]])
            }
          },
          test("mappend left identity") { () =>
            consts.foldMapStrict {
              MonoidLaws.leftIdentity(_)(assertEqual[Const[IList[Int], IList[Double]]])
            }
          },
          test("mappend right identity") { () =>
            consts.foldMapStrict {
              MonoidLaws.rightIdentity(_)(assertEqual[Const[IList[Int], IList[Double]]])
            }
          }
        )
      )
    )
  }
}
