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
      IA.fromArray(Array(2,3)) must haveClass[IA.ofInt]
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

  "Behave like arrays" in {
    "Conversion from array" verifies {(array: Array[Int]) =>
      val ia = IA.fromArray(array)
      if (array.isEmpty) (ia must beEmpty) else (ia must beTheSameSeqAs(array))
      // IA.fromArray(array) must beTheSameSeqAs(array)
    }

    "Appending arrays" verifies {(array1: Array[Int], array2: Array[Int]) =>
      val array = array1 ++ array2
      val ia = IA.fromArray(array1) ++ IA.fromArray(array2)
      if (array.isEmpty) (ia must beEmpty) else (ia must (haveSuperClass[ImmutableArray[_]] and beTheSameSeqAs(array)))
    }

    "Appending strings" verifies {(str1: String, str2: String) =>
      val str = str1 ++ str2
      val ia = IA.fromString(str1) ++ IA.fromString(str2)
      if (str.isEmpty) (ia must beEmpty) else (ia must (haveSuperClass[ImmutableArray[_]] and beTheSameSeqAs(str)))
      // (IA.fromString(str1) ++ IA.fromString(str2)) must (haveClass[IA.ofString] and beTheSameSeqAs(str1 ++ str2))
      // doesn't pass at the moment but would be desirable. Can IA.canBuildFrom be specialized suitably?
    }
  }
}