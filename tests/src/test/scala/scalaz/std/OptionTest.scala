package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Tags._
import org.scalacheck.Prop._
import org.scalacheck.Prop.forAll

object OptionTest extends SpecLite {

  checkAll("Option", order.laws[Option[Int]])
  checkAll("Option @@ First", order.laws[FirstOption[Int]])
  checkAll("Option @@ Last", order.laws[LastOption[Int]])
  checkAll("Option @@ Min", order.laws[MinOption[Int]])
  checkAll("Option @@ Max", order.laws[MaxOption[Int]])

  checkAll("Option", monoid.laws[Option[Int]])
  checkAll("Option", bindRec.laws[Option])
  checkAll("Option", monadPlus.strongLaws[Option])
  checkAll("Option", traverse.laws[Option])
  checkAll("Option", zip.laws[Option])
  checkAll("Option", alt.laws[Option])
  checkAll("Option", isEmpty.laws[Option])
  checkAll("Option", cobind.laws[Option])
  checkAll("Option", align.laws[Option])
  checkAll("Option", semilattice.laws[Option[ISet[Int]]])

  checkAll("Option @@ First", monoid.laws[FirstOption[Int]])
  checkAll("Option @@ Last", monoid.laws[LastOption[Int]])
  checkAll("Option @@ Min", monoid.laws[MinOption[Int]])
  checkAll("Option @@ Max", monoid.laws[MaxOption[Int]])

  checkAll("Band[Option @@ First]", band.laws[FirstOption[Int]])
  checkAll("Band[Option @@ Last]", band.laws[LastOption[Int]])
  checkAll("Band[Option @@ Min]", band.laws[MinOption[Int]])
  checkAll("Band[Option @@ Max]", band.laws[MaxOption[Int]])

  checkAll("Option @@ First", monad.laws[FirstOption])
  checkAll("Option @@ Last", monad.laws[LastOption])
  checkAll("Option @@ Min", monad.laws[MinOption])
  checkAll("Option @@ Max", monad.laws[MaxOption])

  "None is less than anything else" ! forAll { (x: Option[Int]) => Order[Option[Int]].greaterThanOrEqual(x, None) }

  "None is ignored in Option[A]@@Min" ! forAll { (x: Option[Int]) =>
    import syntax.monoid._
    (Min(x) |+| Min(None)) must_=== Min(x)
  }

  "None is ignored in Option[A]@@Max" ! forAll { (x: Option[Int]) =>
    import syntax.monoid._
    (Max(x) |+| Max(None)) must_=== Max(x)
  }

  "lifted Monoid is short-circuiting" in {
    val M: Monoid[Option[Int]] = Monoid.liftMonoid[Option, Int]

    val f: Int => Maybe[(Option[Int], Int)] = i => {
      if (i > 0) Maybe.just((Option(i), i - 1))
      else if (i == 0) Maybe.just((None, i - 1))
      else sys.error("BOOM!")
    }

    M.unfoldrSum(5)(f) must_=== None
  }

  "lifted PlusEmpty is short-circuiting" in {
    import scalaz.std.list._

    val P: PlusEmpty[λ[a => Option[List[a]]]] = PlusEmpty.liftPlusEmpty[Option, List]

    val f: Int => Maybe[(Option[List[Int]], Int)] = i => {
      if (i > 0) Maybe.just((Option(List.range(0, i)), i - 1))
      else if (i == 0) Maybe.just((None, i - 1))
      else sys.error("BOOM!")
    }

    P.unfoldrPsum(5)(f) must_=== None
  }

  "lifted Reducer is short-circuiting" in {
    ApplyTest.unfoldrOptShortCircuiting[Option](None)
  }

  object instances {
    def equal[A: Equal] = Equal[Option[A]]
    def order[A: Order] = Order[Option[A]]
    def semigroup[A: Semigroup] = Monoid[Option[A]]
    def semiLattice[A: SemiLattice] = SemiLattice[Option[A]]
    def bindRec[A] = BindRec[Option]
    def monad[A] = Monad[Option]
    def alt[A] = Alt[Option]

    def monoidFirst[A] = Monoid[Option[A] @@ First]
    def monoidLast[A] = Monoid[Option[A] @@ Last]

    // checking absence of ambiguity
    def equal[A: Order] = Equal[Option[A]]
    def monoid[A: SemiLattice] = Monoid[Option[A]]
  }
}
