package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Tags._
import org.scalacheck.Prop._

class OptionTest extends Spec {

  checkAll("Option", order.laws[Option[Int]])
  checkAll("Option @@ First", order.laws[Option[Int] @@ First])
  checkAll("Option @@ Last", order.laws[Option[Int] @@ Last])
  checkAll("Option @@ Min", order.laws[MinOption[Int]])
  checkAll("Option @@ Max", order.laws[MaxOption[Int]])

  checkAll("Option", monoid.laws[Option[Int]])
  checkAll("Option", monadPlus.strongLaws[Option])
  checkAll("Option", traverse.laws[Option])
  checkAll("Option", isEmpty.laws[Option])
  checkAll("Option", cobind.laws[Option])
  
  checkAll("Option @@ First", monoid.laws[Option[Int] @@ First])
  checkAll("Option @@ Last", monoid.laws[Option[Int] @@ Last])
  checkAll("Option @@ First", monad.laws[FirstOption])
  checkAll("Option @@ Last", monad.laws[LastOption])
  checkAll("Option @@ Min", monad.laws[MinOption])
  checkAll("Option @@ Max", monad.laws[MaxOption])

  "None is less than anything else" in {
    check { forAll { x: Option[Int] => Order[Option[Int]].greaterThanOrEqual(x, None) }}}

  object instances {
    def equal[A: Equal] = Equal[Option[A]]
    def order[A: Order] = Order[Option[A]]
    def semigroup[A: Semigroup] = Monoid[Option[A]]
    def monad[A] = Monad[Option]

    def monoidFirst[A] = Monoid[Option[A] @@ First]
    def monoidLast[A] = Monoid[Option[A] @@ Last]

    // checking absence of ambiguity
    def equal[A: Order] = Equal[Option[A]]
  }
}
