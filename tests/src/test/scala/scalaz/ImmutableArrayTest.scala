package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import org.scalacheck._
import Gen._
import Prop._
import Arbitrary._
import scalacheck.ScalaCheckBinding._
import Scalaz._
import scalacheck.ScalazArbitrary._
import ImmutableArray._

/**
 * Created by IntelliJ IDEA.
 * User: Alexey
 * Date: 04.07.2010
 * Time: 15:56:19
 * To change this template use File | Settings | File Templates.
 */

class ImmutableArrayTest extends Specification with Sugar with ScalaCheck {
  "Arrays are created with correct types" in {
    "Int" in {
      ImmutableArray.fromArray(Array(2,3)) must haveClass[ImmutableArray.ofInt]
    }

    "Boolean" in {
      ImmutableArray.fromArray(Array(true)) must haveClass[ImmutableArray.ofBoolean]
    }

    "String" in {
      val immArray = ImmutableArray.fromArray(Array("a", "b"))
      "Array's type" in {
        immArray must haveClass[ImmutableArray.ofRef[_]] // can't check more precisely due to erasure
        // immArray must not(haveClass[ImmutableArray.ofRef[StringBuilder]]) // doesn't pass
      }
      //// can't check this, because elemManifest can't be accessible outside due to variance
      // "Elements' type" in {
      //   immArray.elemManifest must beA {m => m.erasure must beEqual(classOf[String])}
      // }
    }

    "StringArray" in {
      ImmutableArray.make("abc") must haveClass[ImmutableArray.StringArray]
    }
  }

  "Behave like arrays" in {
    val array = Array(2,3)
    ImmutableArray.fromArray(array) must (contain(2) and not(contain(4)))
    // ImmutableArray.fromArray(array) must beSameSeqAs array // doesn't compile
  }
}