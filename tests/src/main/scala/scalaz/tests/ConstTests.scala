package scalaz
package tests

import scala.{ Boolean, Double, Int, List, Option }

import data._, Scalaz._

import laws._

import testz.{ assert, Harness }

import z._

final class ConstTests {

  val consts = List(
    Const[List[Int], List[Double]](List(1)),
    Const[List[Int], List[Double]](List())
  )

  def cross[A, B](l1: List[A], l2: List[B]): List[(A, B)] =
    l1.flatMap(a1 => l2.map(a2 => (a1, a2)))

  def tests[T](harness: Harness[T]): T = {
    import harness._
    section(
      namedSection("concrete")(
        test("run . apply") { () =>
          val testValue: Int = 1234
          assertEqual(Const.run(Const[Int, List[Int]](testValue)), testValue)
        },
        test("apply . run") { () =>
          val testValue: Const[Int, List[Int]] =
            Const[Int, List[Int]](2134)
          assertEqual(Const[Int, List[Int]](Const.run(testValue)), testValue)
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
            cross(consts, consts).foldMap {
              case (c1, c2) =>
                assert((c1 === c2) === (Const.run(c1) === Const.run(c2)))
            }
          }
        ),
        namedSection("applicative laws")(
          test("apply associativity") { () =>
            consts.foldMap(
              ApplyLaws.applyAssoc(_)(
                Const[List[Int], List[Double] => List[Boolean]](List(0)),
                Const[List[Int], List[Boolean] => Int](List(1))
              )(assertEqual[Const[List[Int], Int]](_, _))
            )
          },
          test("applicative identity") { () =>
            consts.foldMap {
              ApplicativeLaws.applyIdentity(_)(assertEqual[Const[List[Int], List[Double]]])
            }
          }
        ),
        namedSection("traversable laws")(
          test("traversable composition") { () =>
            val fst = (a: List[Double]) => a.headOption
            val snd = (a: Double) => if (a > 0.0) scala.Some(a) else scala.None

            consts.foldMap {
              TraversableLaws.traverseComposition(_)(fst, snd)(assertEqual[Option[Option[Const[List[Int], Double]]]])
            }
          },
          test("traversable identity") { () =>
            consts.foldMap {
              TraversableLaws.traverseIdentity(_)(assertEqual[Const[List[Int], List[Double]]])
            }
          }
        ),
        namedSection("monoid laws")(
          test("mappend associativity") { () =>
            cross(cross(consts, consts), consts).foldMap {
              case ((l1, l2), l3) =>
                SemigroupLaws.assoc(l1, l2, l3)(assertEqual[Const[List[Int], List[Double]]])
            }
          },
          test("mappend left identity") { () =>
            consts.foldMap {
              MonoidLaws.leftIdentity(_)(assertEqual[Const[List[Int], List[Double]]])
            }
          },
          test("mappend right identity") { () =>
            consts.foldMap {
              MonoidLaws.rightIdentity(_)(assertEqual[Const[List[Int], List[Double]]])
            }
          }
        )
      )
    )
  }
}
