package scalaz

import org.scalacheck.Arbitrary
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import Cord._

class CordTest extends Spec {
  "split() must result in two cords whose summary length is equal to the length of original cord " in {
    val x = Cord("Once upon a midnight dreary")
    for (i <- 0 until x.length) {
      val split = x split i
      split._1.length + split._2.length must_== x.length
    }
  }

  "drop() must make cord shorter" in {
    val theString = "While I pondered, weak and weary"
    val x = Cord(theString)
    for (i <- 0 until x.length) {
      val y = x drop i
      y.toString must_== theString.substring(i)
    }
  }

  "tail() must be smaller than the whole, generally" in {
    val x = Cord("abc")
    x.tail.toString must_== "bc"
    x.tail.tail.toString must_== "c"
    x.tail.tail.tail.toString must_== ""
  }

  implicit def ArbitraryCord: Arbitrary[Cord] = Functor[Arbitrary].map(implicitly[Arbitrary[String]])(Cord.stringToCord)

  checkAll(monoid.laws[Cord])
  checkAll(equal.laws[Cord])
}
