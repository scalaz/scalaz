package scalaz
package iteratee

import std.AllInstances._
import Iteratee._
import Enumeratee2T._
import effect._

class Enumeratee2TTest extends Spec {
  "join equal pairs" in {
    implicit val v = IterateeT.IterateeTMonad[Unit, Int, Id]
    type IterateeM[A] = IterateeT[Unit, Int, Id, A]

    val enum  = enumStream[Unit, Int, IterateeM](Stream(1, 3, 5, 7)) 
    val enum2 = enumStream[Unit, Int, Id](Stream(2, 3, 4, 5, 6)) 

    val outer = joinI[Unit, Int, Int, Id].apply(consume[Unit, (Int, Int), Id, List].value) &= enum
    val inner = outer.run(_ => sys.error("...")) &= enum2

    inner.run(_ => sys.error("...")).pointI.run(_ => sys.error("...")) must be_===(List((3, 3), (5, 5)))
  }

  "cogroup" in {
    import Either3._
    import IdT._
    implicit val ls = listShow[Either3[Int, (Int, Int), Int]]
    implicit val v = IterateeT.IterateeTMonad[Unit, Int, Id]
    type IterateeM[A] = IterateeT[Unit, Int, Id, A]

    val enum  = enumStream[Unit, Int, IterateeM](Stream(1, 3, 3, 5, 7, 8, 8)) 
    val enum2 = enumStream[Unit, Int, Id](Stream(2, 3, 4, 5, 5, 6, 8, 8)) 

    val outer = cogroupI[Unit, Int, Int, Id].apply(consume[Unit, Either3[Int, (Int, Int), Int], Id, List].value) &= enum
    val inner = outer.run(_ => sys.error("...")) &= enum2
    
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

  "merge sorted iteratees" in {
    implicit val v = IterateeT.IterateeTMonad[Unit, Int, Id]
    type IterateeM[α] = IterateeT[Unit, Int, Id, α]

    val enum  = enumStream[Unit, Int, IterateeM](Stream(1, 3, 5)) 
    val enum2 = enumStream[Unit, Int, Id](Stream(2, 3, 3, 4, 5, 6)) 

    val outer = mergeI[Unit, Int, Id].apply(consume[Unit, Int, Id, List].value) &= enum
    val inner = outer.run(_ => sys.error("...")) &= enum2
    
    inner.run(_ => sys.error("...")).pointI.run(_ => sys.error("...")) must be_===(List(1, 2, 3, 3, 3, 4, 5, 5, 6))
  }

  "cross the first element with all of the second iteratee's elements" in {
    import IdT._
    implicit val v = IterateeT.IterateeTMonad[Unit, Int, Id]
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
