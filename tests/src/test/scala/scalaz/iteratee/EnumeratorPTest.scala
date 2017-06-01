package scalaz
package iteratee

import std.AllInstances._
import Either3._
import Iteratee._

import Id._

object EnumeratorPTest extends SpecLite {
  implicit val intO = Order[Int].order _
  "cogroupE" should {
    "work the same as directly using the nested iteratee " in {
      val enum  = enumPStream[Int, Id](Stream(1, 3, 3, 5, 7, 8, 8))
      val enum2 = enumPStream[Int, Id](Stream(2, 3, 4, 5, 5, 6, 8, 8))

      val cf = cogroupE[Int, Int, Id]
      val enumR = cf(enum, enum2)

      (consume[Either3[Int, (Int, Int), Int], Id, List] &= enumR.apply[Id]).run must_===(List(
        left3(1),
        right3(2),
        middle3((3, 3)),
        middle3((3, 3)),
        right3(4),
        middle3((5, 5)),
        middle3((5, 5)),
        right3(6),
        left3(7),
        middle3((8, 8)),
        middle3((8, 8)),
        middle3((8, 8)),
        middle3((8, 8))
      ))
    }

    "compose" in {
      val enum  = enumPStream[Int, Id](Stream(1, 3, 3, 5, 7, 8, 8))
      val enum2 = enumPStream[Int, Id](Stream(2, 3, 4, 5, 5, 6, 8, 8))
      val enum3 = enumPStream[Int, Id](Stream(3, 5, 8))

      val cf = cogroupE[Int, Int, Id]
      val enumR = cf(cf(enum, enum2) map { _.fold(identity[Int], _._1, identity[Int]) }, enum3)

      (consume[Either3[Int, (Int, Int), Int], Id, List] &= enumR.apply[Id]).run must_===(List(
        left3(1),
        left3(2),
        middle3((3, 3)),
        middle3((3, 3)),
        left3(4),
        middle3((5, 5)),
        middle3((5, 5)),
        left3(6),
        left3(7),
        middle3((8, 8)),
        middle3((8, 8)),
        middle3((8, 8)),
        middle3((8, 8))
      ))
    }
  }

  "mergeAll" in {
    val enum1 = enumPStream[Int, Id](Stream(1, 5, 9))
    val enum2 = enumPStream[Int, Id](Stream(2, 3, 6))
    val enum3 = enumPStream[Int, Id](Stream(4, 7, 8))
    (consume[Int, Id, List] &= mergeAll(enum1, enum2, enum3).apply[Id]).run must_===(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }
}

// vim: set ts=4 sw=4 et:
