package scalaz

import std.tuple._
import syntax.associative._

object AssociativeTest extends SpecLite {

  def compilationTest: Unit = {
    val a1 = (1, (2, 3))
    val b1 = a1.reassociateLeft
    b1: ((Int, Int), Int)

    val a2: (Int \/ Int) \/ Int = \/-(42)
    val b2 = a2.reassociateRight
    b2: (Int \/ (Int \/ Int))
  }

}
