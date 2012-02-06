package scalaz
package iteratee

import std.AllInstances._
import Iteratee._
import Enumeratee2T._
import effect._
import Either3._
import MonadPartialOrder._

class Enumeratee2TTest extends Spec {
  implicit val ls = listShow[Either3[Int, (Int, Int), Int]]
  implicit val v = IterateeT.IterateeTMonad[Unit, Int, Id]
  implicit val vt = IterateeT.IterateeTMonadTrans[Unit, Int]
  implicit val mpo = MonadPartialOrder.transformer[Id, ({ type λ[β[_], α] = IterateeT[Unit, Int, β, α] })#λ]

  type StepM[A] = StepT[Unit, Int, Id, A]
  type IterateeM[A] = IterateeT[Unit, Int, Id, A]

  "join equal pairs" in {
    val enum  = enumStream[Unit, Int, IterateeM](Stream(1, 3, 5, 7)) 
    val enum2 = enumStream[Unit, Int, Id](Stream(2, 3, 4, 5, 6)) 

    val outer = joinI[Unit, Int, Int, Id].apply(consume[Unit, (Int, Int), Id, List].value) &= enum
    val inner = outer.run(_ => sys.error("...")) &= enum2

    inner.run(_ => sys.error("...")).pointI.run(_ => sys.error("...")) must be_===(List((3, 3), (5, 5)))
  }

  "cogroup" should {
    type E3I = Either3[Int, (Int, Int), Int]
    type E3LI = List[E3I]
    "match equal elements, retaining unequal elements on the \"side\" they came from" in {
      val enum  = enumStream[Unit, Int, IterateeM](Stream(1, 3, 3, 5, 7, 8, 8)) 
      val enum2 = enumStream[Unit, Int, Id](Stream(2, 3, 4, 5, 5, 6, 8, 8)) 

      val consumer = consume[Unit, E3I, Id, List]
      val outer = consumer.advance[Int, StepT[Unit, E3I, Id, E3LI], IterateeM](cogroupI[Unit, Int, Int, Id].apply[E3LI])(mpo)
      val outer2 = outer &= enum
      val inner = outer2.run(_ => sys.error("...")) &= enum2
      
      inner.run(_ => sys.error("...")).pointI.run(_ => sys.error("...")) must_== List[Either3[Int, (Int, Int), Int]](
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
    val enum  = enumStream[Unit, Int, IterateeM](Stream(1, 3, 5)) 
    val enum2 = enumStream[Unit, Int, Id](Stream(2, 3, 3, 4, 5, 6)) 

    val outer = mergeI[Unit, Int, Id].apply(consume[Unit, Int, Id, List].value) &= enum
    val inner = outer.run(_ => sys.error("...")) &= enum2
    
    inner.run(_ => sys.error("...")).pointI.run(_ => sys.error("...")) must be_===(List(1, 2, 3, 3, 3, 4, 5, 5, 6))
  }

  "cross the first element with all of the second iteratee's elements" in {
    val enum1 = enumStream[Unit, Int, Id](Stream(1, 3, 5)) 
    val enum2 = enumStream[Unit, Int, Id](Stream(2, 3, 4)) 

    val consumer = consume[Unit, (Int, Int), Id, List]
    val producer = cross[Unit, Int, Int, Id](enum1, enum2)
    (consumer &= producer).run(_ => sys.error("...")) must be_===(List(
      (1, 2), (1, 3), (1, 4), (3, 2), (3, 3), (3, 4), (5, 2), (5, 3), (5, 4)
    ))
  }

  "join the first element with all of the second iteratee's elements, which compare equal" in {
    val enum1p = new EnumeratorP[Unit, Int, Id] {
      def apply[F[_]](implicit ord: MonadPartialOrder[F, Id]): EnumeratorT[Unit, Int, F] = {
        import ord._
        enumStream[Unit, Int, F](Stream(1)) 
      }
    }

    val enum2p = new EnumeratorP[Unit, Int, Id] {
      def apply[F[_]](implicit ord: MonadPartialOrder[F, Id]): EnumeratorT[Unit, Int, F] = {
        import ord._
        enumStream[Unit, Int, F](Stream(1, 1, 1)) 
      }
    }

    val consumer = consume[Unit, (Int, Int), Id, List]
    val producer = joinE[Unit, Int, Int, Id].apply(enum1p, enum2p).apply[Id]
    (consumer &= producer).run(_ => sys.error("...")) must be_===(List(
      (1, 1), (1, 1), (1, 1)
    ))
  }
}

// vim: set ts=4 sw=4 et:
