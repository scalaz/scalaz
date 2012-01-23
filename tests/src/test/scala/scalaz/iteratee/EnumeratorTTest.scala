package scalaz
package iteratee

import std.AllInstances._
import Iteratee._
import effect._

class EnumeratorTTest extends Spec {
  "eof" in {
    val enum = enumEofT[Unit, Int, Id]
    (consume[Unit, Int, Id, List] &= enum).run(_ => List(1)) must be_===(Nil)
  }
}


// vim: set ts=4 sw=4 et:
