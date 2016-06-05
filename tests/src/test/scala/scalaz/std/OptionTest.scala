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
  checkAll("Option", isEmpty.laws[Option])
  checkAll("Option", cobind.laws[Option])
  checkAll("Option", align.laws[Option])

  checkAll("Option @@ First", monoid.laws[FirstOption[Int]])
  checkAll("Option @@ Last", monoid.laws[LastOption[Int]])
  checkAll("Option @@ Min", monoid.laws[MinOption[Int]])
  checkAll("Option @@ Max", monoid.laws[MaxOption[Int]])

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

  object instances {
    def equal[A: Equal] = Equal[Option[A]]
    def order[A: Order] = Order[Option[A]]
    def semigroup[A: Semigroup] = Monoid[Option[A]]
    def bindRec[A] = BindRec[Option]
    def monad[A] = Monad[Option]

    def monoidFirst[A] = Monoid[Option[A] @@ First]
    def monoidLast[A] = Monoid[Option[A] @@ Last]

    // checking absence of ambiguity
    def equal[A: Order] = Equal[Option[A]]
  }
}
