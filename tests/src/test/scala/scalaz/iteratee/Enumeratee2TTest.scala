package scalaz
package iteratee

import std.AllInstances._
import Iteratee._
import Enumeratee2T._
import Either3._
import Id._
import NaturalTransformation.id

object Enumeratee2TTest extends SpecLite {
  implicit val vt = IterateeT.IterateeTMonadTrans[Int]
  val intO = Order[Int].order _

  type StepM[A] = StepT[Int, Id, A]
  type IterateeM[A] = IterateeT[Int, Id, A]

  val vtLift = new (Id ~> IterateeM) {
    def apply[A](a: Id[A]): IterateeM[A] =
      vt.liftM(a)
  }

  "join equal pairs" in {
    val enum1 = enumStream[Int, IterateeM](Stream(1, 3, 5, 7))
    val enum2 = enumStream[Int, Id](Stream(2, 3, 4, 5, 6))

    val outer = joinI[Int, Int, Id](intO).apply(consume[(Int, Int), Id, List].value) &= enum1
    val inner = outer.run &= enum2

    inner.run.pointI.run must_===(List((3, 3), (5, 5)))
  }

  "cogroup" should {
    type E3I = Either3[Int, (Int, Int), Int]
    type E3LI = List[E3I]
    "match equal elements, retaining unequal elements on the \"side\" they came from" in {
      val enum1 = enumStream[Int, IterateeM](Stream(1, 3, 3, 5, 7, 8, 8))
      val enum2 = enumStream[Int, Id](Stream(2, 3, 4, 5, 5, 6, 8, 8))

      val consumer = consume[E3I, Id, List]
      val outer = consumer.advance[Int, StepT[E3I, Id, E3LI], IterateeM](cogroupI[Int, Int, Id](intO).apply[E3LI], vtLift)
      val outer2 = outer &= enum1
      val inner = outer2.run &= enum2

      inner.run.pointI.run must_== List[Either3[Int, (Int, Int), Int]](
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
      )
    }
  }

  "merge sorted iteratees" in {
    val enum1 = enumStream[Int, IterateeM](Stream(1, 3, 5))
    val enum2 = enumStream[Int, Id](Stream(2, 3, 3, 4, 5, 6))

    val outer = mergeI[Int, Id].apply(consume[Int, Id, List].value) &= enum1
    val inner = outer.run &= enum2

    inner.run.pointI.run must_===(List(1, 2, 3, 3, 3, 4, 5, 5, 6))
  }

  "cross the first element with all of the second iteratee's elements" in {
    val enum1 = enumStream[Int, Id](Stream(1, 3, 5))
    val enum2 = enumStream[Int, Id](Stream(2, 3, 4))

    val consumer = consume[(Int, Int), Id, List]
    val producer = enum1 cross enum2
    (consumer &= producer).run must_===(List(
      (1, 2), (1, 3), (1, 4), (3, 2), (3, 3), (3, 4), (5, 2), (5, 3), (5, 4)
    ))
  }

  "join the first element with all of the second iteratee's elements, which compare equal" in {
    val enum1p = new EnumeratorP[Int, Id] {
      def apply[F[_]: Monad](trans: Id ~> F): EnumeratorT[Int, F] =
        enumStream[Int, F](Stream(1))
    }

    val enum2p = new EnumeratorP[Int, Id] {
      def apply[F[_]: Monad](trans: Id ~> F): EnumeratorT[Int, F] =
        enumStream[Int, F](Stream(1, 1, 1))
    }

    val consumer = consume[(Int, Int), Id, List]
    val producer = joinE[Int, Int, Id](intO).apply(enum1p, enum2p).apply[Id](id)
    (consumer &= producer).run must_===(List(
      (1, 1), (1, 1), (1, 1)
    ))
  }
}

// vim: set ts=4 sw=4 et:
