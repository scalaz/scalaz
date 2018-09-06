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
      namedSection("concrete")(
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
      namedSection("laws")(
        namedSection("eq laws")(
          test("reflexivity") { () =>
            consts.foldMap(
              EqLaws.reflexivity(_)(assert)
            )
          },
          test("identity") { () =>
            consts.cross(consts).foldMap {
              case (c1, c2) =>
                assert((c1 === c2) === (Const.run(c1) === Const.run(c2)))
            }
          }
        ),
        namedSection("applicative laws")(
          test("apply associativity") { () =>
            consts.foldMap(
              ApplyLaws.applyAssoc(_)(
                Const[IList[Int], IList[Double] => IList[Boolean]](IList(0)),
                Const[IList[Int], IList[Boolean] => Int](IList(1))
              )(assertEqual[Const[IList[Int], Int]](_, _))
            )
          },
          test("applicative identity") { () =>
            consts.foldMap {
              ApplicativeLaws.applyIdentity(_)(assertEqual[Const[IList[Int], IList[Double]]])
            }
          }
        ),
        namedSection("traversable laws")(
          test("traversable composition") { () =>
            val fst = (_: IList[Double]).head
            val snd = (a: Double) => if (a > 0.0) Maybe.just(a) else Maybe.empty[Double]

            consts.foldMap {
              TraversableLaws.traverseComposition(_)(fst, snd)(assertEqual[Maybe[Maybe[Const[IList[Int], Double]]]])
            }
          },
          test("traversable identity") { () =>
            consts.foldMap {
              TraversableLaws.traverseIdentity(_)(assertEqual[Const[IList[Int], IList[Double]]])
            }
          }
        ),
        namedSection("monoid laws")(
          test("mappend associativity") { () =>
            consts.cross(consts).cross(consts).foldMap {
              case ((l1, l2), l3) =>
                SemigroupLaws.assoc(l1, l2, l3)(assertEqual[Const[IList[Int], IList[Double]]])
            }
          },
          test("mappend left identity") { () =>
            consts.foldMap {
              MonoidLaws.leftIdentity(_)(assertEqual[Const[IList[Int], IList[Double]]])
            }
          },
          test("mappend right identity") { () =>
            consts.foldMap {
              MonoidLaws.rightIdentity(_)(assertEqual[Const[IList[Int], IList[Double]]])
            }
          }
        )
      )
    )
  }
}
