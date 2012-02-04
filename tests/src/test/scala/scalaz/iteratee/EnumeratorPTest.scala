package scalaz
package iteratee

import std.AllInstances._
import Iteratee._
import effect._

import org.scalacheck.{Pretty, Gen, Arbitrary}
import Arbitrary._
import Gen._
import syntax.functor._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class EnumeratorPTest extends Spec {
  "mergeAll" in {
    val enum1 = enumPStream[Unit, Int, Id](Stream(1, 5, 9))
    val enum2 = enumPStream[Unit, Int, Id](Stream(2, 3, 6))
    val enum3 = enumPStream[Unit, Int, Id](Stream(4, 7, 8))
    (consume[Unit, Int, Id, List] &= mergeAll(enum1, enum2, enum3).apply[Id]).runOrZero must be_===(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }
}

// vim: set ts=4 sw=4 et:
