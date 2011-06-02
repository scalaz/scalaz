package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import org.scalacheck._
import Gen._
import Prop._
import Arbitrary._
import scalacheck.ScalaCheckBinding._
import Scalaz._
import scalacheck.ScalazArbitrary._
import scalaz.{ImmutableArray => IA}

class ImmutableArrayTest extends Specification with Sugar with ScalaCheck {
  "Arrays are created with correct types" in {
    "Int" in {
      IA.fromArray(Array(2, 3)) must haveClass[IA.ofInt]
    }

    "Boolean" in {
      IA.fromArray(Array(true)) must haveClass[IA.ofBoolean]
    }

    "String" in {
      val immArray = IA.fromArray(Array("a", "b"))
      "Array's type" in {
        immArray must haveClass[IA.ofRef[_]] // can't check more precisely due to erasure
        // immArray must not(haveClass[ImmutableArray.ofRef[StringBuilder]]) // doesn't pass
      }
      //// can't check this, because elemManifest can't be accessible outside due to variance
      // "Elements' type" in {
      //   immArray.elemManifest must beA {m => m.erasure must beEqual(classOf[String])}
      // }
    }

    "StringArray" in {
      IA.make("abc") must haveClass[IA.StringArray]
    }
  }
}