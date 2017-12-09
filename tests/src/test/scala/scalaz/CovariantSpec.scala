// Copyright: 2017 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

package scalaz

import java.lang.String

import org.scalatest._

import examples.adt._
import examples.recadt._
import examples.recgadt._

class CovariantSpec extends FlatSpec with NonImplicitAssertions {
  import Matchers._

  "products" should "behave as expected" in {
    Default[Faz].default should equal(Faz(false, 0))
  }

  "coproducts" should "behave as expected" in {
    Default[Foo].default should equal(Bar(""))
  }

  // Default for a recursive ADT is a dumb idea It only works by accident here
  // because of the ordering of the source code case classes! Try using \/- as
  // the choice, or swap the case classes around and watch the world explode
  // with an infinite loop.
  "recursive products" should "behave as expected" in {
    Default[Leaf].default should equal(Leaf(""))
  }

  "recursive coproducts" should "behave as expected" in {
    Default[ATree].default should equal(Leaf(""))
  }

  "recursive GADT products" should "behave as expected" in {
    Default[GLeaf[String]].default should equal(GLeaf(""))
  }

  "recursive GADT coproducts" should "behave as expected" in {
    Default[GTree[String]].default should equal(GLeaf(""))
  }

}
