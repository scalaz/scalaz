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

  "map" in {
    val enum = enumStream[Unit, Int, Id](Stream(1, 2, 3))
    type EnumId[α] = EnumeratorT[Unit, α, Id]
    (consume[Unit, Int, Id, List] &= Functor[EnumId].map(enum){ _ * 2 }).run(_ => List.empty[Int]) must be_===(List(2, 4, 6))
  }
}


// vim: set ts=4 sw=4 et:
