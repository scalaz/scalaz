package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._


object TheseTTest extends SpecLite {
  type TheseTList[A, B] = TheseT[List, A, B]
  type TheseTListInt[A] = TheseT[List, Int, A]
  type TheseTOptionInt[A] = TheseT[Option, Int, A]

  checkAll(monad.laws[TheseTListInt])
  checkAll(traverse.laws[TheseTListInt])
  checkAll(bitraverse.laws[TheseTList])

}
