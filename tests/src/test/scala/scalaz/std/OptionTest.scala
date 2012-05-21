package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Tags._

class OptionTest extends Spec {

  checkAll("Option", order.laws[Option[Int]])
  checkAll("Option @@ First", order.laws[Option[Int] @@ First])
  checkAll("Option @@ Last", order.laws[Option[Int] @@ Last])

  checkAll("Option", monoid.laws[Option[Int]])
  checkAll("Option", monadPlus.strongLaws[Option])
  checkAll("Option", traverse.laws[Option])
  
  checkAll("Option @@ First", monoid.laws[Option[Int] @@ First])
  checkAll("Option @@ Last", monoid.laws[Option[Int] @@ Last])
  checkAll("Option @@ First", monad.laws[({type f[x] = Option[x] @@ First})#f])
  checkAll("Option @@ Last", monad.laws[({type f[x] = Option[x] @@ Last})#f])

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
