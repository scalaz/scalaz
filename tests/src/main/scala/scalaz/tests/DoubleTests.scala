package scalaz
package tests

import Scalaz._

import scala.{ Double, Long }
import scala.Predef.double2Double
import data.IList
import laws.EqLaws
import tests.z.resultMonoid
import tests.z.assertEqual
import testz.{ assert, Harness, Succeed }

class DoubleTests {
  final val doubleNaN           = java.lang.Double.longBitsToDouble(0x7ff8000000000000L)
  final val doubleNaN1          = java.lang.Double.longBitsToDouble(0x7ff8000000000001L)
  final val doubleMinusNaN      = java.lang.Double.longBitsToDouble(0xfff8000000000000L)
  final val doubleInfinity      = java.lang.Double.longBitsToDouble(0x7ff0000000000000L)
  final val doubleMinusInfinity = java.lang.Double.longBitsToDouble(0xfff0000000000000L)

  val doubles = IList.apply(-0.0d,
                            0.0d,
                            1.0d,
                            -1.0d,
                            doubleNaN,
                            doubleNaN1,
                            doubleMinusNaN,
                            doubleInfinity,
                            java.lang.Double.MIN_VALUE,
                            java.lang.Double.MAX_VALUE,
                            doubleMinusInfinity)

  val observations = IList[Double => Long](
    x => if (x > 0) 1 else 0,
    x => if (x == 0) 1 else 0,
    x => if (java.lang.Double.isNaN(x)) 1 else 0,
    x => if (java.lang.Double.isInfinite(x) && x > 0) 1 else 0,
    x => if (java.lang.Double.isInfinite(x) && x < 0) 1 else 0,
    x => x.longValue(),
    x => if ((1.0 / x) > 0) 1 else 0
  )

  def tests[T](harness: Harness[T]): T = {
    import harness._

    section("laws")(
      section("eq laws")(
        test("reflexivity") { () =>
          doubles.foldMap(
            EqLaws.reflexivity(_)(assert)
          )
        },
        test("identity") { () =>
          doubles.cross(doubles).cross(observations).foldMap {
            case ((a, b), obs) =>
              EqLaws.identity(a, b)(obs)(
                (equal, leftObs, rightObs) => if (equal) assertEqual(leftObs, rightObs) else Succeed()
              )
          }
        }
      )
    )
  }
}
