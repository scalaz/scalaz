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
  checkAll("Option", monadPlus.laws[Option])
  checkAll("Option", traverse.laws[Option])
  checkAll("Option", alternativeEmpty.laws[Option])
  
  checkAll("Option @@ First", monoid.laws[Option[Int] @@ First])
  checkAll("Option @@ Last", monoid.laws[Option[Int] @@ Last])
  checkAll("Option @@ First", monad.laws[({type f[x] = Option[x] @@ First})#f])
  checkAll("Option @@ Last", monad.laws[({type f[x] = Option[x] @@ Last})#f])
}
