package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Arbitrary
import Tags._
import syntax.contravariant._

object AnyValTest extends SpecLite {

  private[this] implicit def tagArb[A, B](implicit A: Arbitrary[A]): Arbitrary[A @@ B] =
    Functor[Arbitrary].map(A)(Tag.apply[A, B])

  checkAll("Unit", order.laws[Unit])
  checkAll("Boolean", order.laws[Boolean].withProp("benchmark", order.scalaOrdering[Boolean]))
  checkAll("Char", order.laws[Char].withProp("benchmark", order.scalaOrdering[Char]))
  checkAll("Short", order.laws[Short].withProp("benchmark", order.scalaOrdering[Short]))
  checkAll("Int", order.laws[Int].withProp("benchmark", order.scalaOrdering[Int]))
  checkAll("Long", order.laws[Long].withProp("benchmark", order.scalaOrdering[Long]))
  checkAll("Float", order.laws[Float].withProp("benchmark", order.scalaOrdering[Float]))
  checkAll("Int @@ Multiplication", order.laws[Int @@ Multiplication])
  checkAll("Boolean @@ Conjunction", order.laws[Boolean @@ Conjunction])
  checkAll("Char @@ Multiplication", order.laws[Char @@ Multiplication])
  checkAll("Byte @@ Multiplication", order.laws[Byte @@ Multiplication])
  checkAll("Long @@ Multiplication", order.laws[Long @@ Multiplication])
  checkAll("Short @@ Multiplication", order.laws[Short @@ Multiplication])

  checkAll("Boolean @@ Conjunction", monoid.laws[Boolean @@ Conjunction])

  checkAll("Band[Boolean @@ Conjunction]", band.laws[Boolean @@ Conjunction])
  checkAll("Band[Boolean @@ Disjunction]", band.laws[Boolean @@ Disjunction])

  {
    implicit val B = std.anyVal.booleanInstance.conjunction
    checkAll("Boolean", monoid.laws[Boolean])
  }

  {
    implicit val B = std.anyVal.booleanInstance.disjunction
    checkAll("Boolean", monoid.laws[Boolean])
  }

  checkAll("Int @@ Multiplication", monoid.laws[Int @@ Multiplication])
  checkAll("Short @@ Multiplication", monoid.laws[Short @@ Multiplication])
  checkAll("Byte", monoid.laws[Byte])
  checkAll("Byte @@ Multiplication", monoid.laws[Byte @@ Multiplication])
  checkAll("Long @@ Multiplication", monoid.laws[Long @@ Multiplication])

  checkAll("Unit", semilattice.laws[Unit])

  checkAll("Unit", monoid.laws[Unit])
  checkAll("Int", monoid.laws[Int])
  checkAll("Short", monoid.laws[Short])
  checkAll("Long", monoid.laws[Long])

  checkAll("Unit", enum.laws[Unit])
  checkAll("Boolean", enum.laws[Boolean])
  checkAll("Char", enum.laws[Char])
  checkAll("Short", enum.laws[Short])
  checkAll("Int", enum.laws[Int])
  checkAll("Long", enum.laws[Long])
  checkAll("Int @@ Multiplication", enum.laws[Int @@ Multiplication])
  checkAll("Boolean @@ Conjunction", enum.laws[Boolean @@ Conjunction])
  checkAll("Char @@ Multiplication", enum.laws[Char @@ Multiplication])
  checkAll("Byte @@ Multiplication", enum.laws[Byte @@ Multiplication])
  checkAll("Long @@ Multiplication", enum.laws[Long @@ Multiplication])
  checkAll("Short @@ Multiplication", enum.laws[Short @@ Multiplication])

  "Int multiplication should terminate when encountering 0" in {
    val M = Monoid[Int @@ Multiplication]
    implicit val S: Show[Int @@ Multiplication] = Show[Int].contramap(Tag.unwrap)

    val f: Int => Maybe[(Int, Int @@ Multiplication)] = i => {
      if(i >= 0) Maybe.just((i-1, Tag(i)))
      else sys.error("BOOM!")
    }
    val g = (i: Int) => f(i) map (_.swap)

    M.unfoldlSum(5)(f) must_=== Tag(0)
    M.unfoldrSum(5)(g) must_=== Tag(0)
  }

  "conjunction should terminate when encountering false" in {
    val M = booleanInstance.conjunction

    val f: Int => Maybe[(Int, Boolean)] = i => {
      if(i > 0) Maybe.just((i-1, true))
      else if(i == 0) Maybe.just((i-1, false))
      else sys.error("BOOM!")
    }
    val g = (i: Int) => f(i) map (_.swap)

    M.unfoldlSum(5)(f) must_=== false
    M.unfoldrSum(5)(g) must_=== false
  }

  "disjunction should terminate when encountering true" in {
    val M = booleanInstance.disjunction

    val f: Int => Maybe[(Int, Boolean)] = i => {
      if(i > 0) Maybe.just((i-1, false))
      else if(i == 0) Maybe.just((i-1, true))
      else sys.error("BOOM!")
    }
    val g = (i: Int) => f(i) map (_.swap)

    M.unfoldlSum(5)(f) must_=== true
    M.unfoldrSum(5)(g) must_=== true
  }
}
