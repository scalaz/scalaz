package scalaz
package iteratee

import std.AllInstances._
import Iteratee._
import effect._

import org.scalacheck.Arbitrary
import scalaz.scalacheck.ScalaCheckBinding._
import Id._

object EnumeratorTTest extends SpecLite {
  implicit def enumeratorTArb[F[_], A](implicit FA: Arbitrary[List[A]], F: Monad[F]): Arbitrary[EnumeratorT[A, F]] = Functor[Arbitrary].map(FA)(l => EnumeratorT.enumStream[A, F](l.toStream))

  implicit def enumeratorEqual[A](implicit EQ: Equal[A]): Equal[Enumerator[A]] = new Equal[Enumerator[A]] {
    def equal(en1: Enumerator[A], en2: Enumerator[A]): Boolean = {
      val l1 = (consume[A, Id, List] &= en1).run
      val l2 = (consume[A, Id, List] &= en2).run
      Equal[List[A]].equal(l1, l2)
    }
  }

  "Issue #553" in {
    import std.list._
    val xs = (1 to 10).map(List(_)).toList
    val e = enumIterator(xs.iterator)
    (Iteratee.sum[List[Int], IO] &= e).run.unsafePerformIO must_===(xs.flatten)
    (Iteratee.sum[List[Int], IO] &= e).run.unsafePerformIO must_===(xs.flatten)
  }

  "eof" in {
    val enum = enumEofT[Int, Id]
    (consume[Int, Id, List] &= enum).run must_===(Nil)
  }

  "map" in {
    val enum = enumStream[Int, Id](Stream(1, 2, 3))
    (consume[Int, Id, List] &= enum.map(_ * 2)).run must_===(List(2, 4, 6))
  }

  "flatMap" in {
    val enum = enumStream[Int, Id](Stream(1, 2, 3))
    (consume[Int, Id, List] &= enum.flatMap(i => enum.map(_ + i))).run must_===(List(2, 3, 4, 3, 4, 5, 4, 5, 6))
  }

  "flatten in a generalized fashion" in {
    val enum = enumOne[List[Int], List](List(1, 2, 3))
    (consume[Int, List, List] &= enum.flatten).run.flatten must_===(List(1, 2, 3))
  }

  "uniq" in {
    val enum = enumStream[Int, Id](Stream(1, 1, 2, 2, 2, 3, 3))
    (consume[Int, Id, List] &= enum.uniq).run must_===(List(1, 2, 3))
  }

  "zipWithIndex" in {
    val enum = enumStream[Int, Id](Stream(3, 4, 5))
    (consume[(Int, Long), Id, List] &= enum.zipWithIndex).run must_===(List((3, 0L), (4, 1L), (5, 2L)))
  }

  "zipWithIndex" in {
    val enum = enumStream[Int, Id](Stream(3, 4, 5))
    (consume[(Int, Long), Id, List] &= enum.zipWithIndex).run must_===(List((3, 0L), (4, 1L), (5, 2L)))
  }

  "zipWithIndex in combination with another function" in {
    val enum = enumStream[Int, Id](Stream(3, 4, 4, 5))
    (consume[(Int, Long), Id, List] &= enum.uniq.zipWithIndex).run must_===(List((3, 0L), (4, 1L), (5, 2L)))
  }

  "lift" in {
    val enum = EnumeratorT.enumeratorTMonadTrans.liftM(List(1, 2, 3))
    (collectT[Int, List, Id] &= enum.map(_ * 2)).run must_===(List(2, 4, 6))
  }

  "enumerate an array" in {
    val enum = enumArray[Int, Id](Array(1, 2, 3, 4, 5), 0, Some(3))
    (consume[Int, Id, List] &= enum).run must_===(List(1, 2, 3))
  }

  "allow for nesting of monads" in {
    type OIO[α] = OptionT[IO, α]
    val enum = enumIterator[Int, OIO](List(1, 2, 3).iterator)
    (consume[Int, OIO, List] &= enum.map(_ * 2)).run.run.unsafePerformIO() must_===(Some(List(2, 4, 6)))
  }

  "drain" in {
    val enum = enumStream[Int, Id](Stream(1, 2, 3))
    enum.drainTo[List] must_===(List(1, 2, 3))
  }

  "perform an interleaved effect" in {
    import scalaz.syntax.monoid._
    var v: Int = 0
    val enum = enumStream[Int, IO](Stream(1, 2))
    val effect = EnumeratorT.perform[Int, IO, Unit](IO(v = 1))
    val enum2 = enumStream[Int, IO](Stream(3, 4))

    val testIter = IterateeT.fold[Int, IO, Boolean](true) {
      case (false, _) => false
      case (true, i) => if (i <= 2) v == 0 else v == 1
    }

    (testIter &= (enum |+| effect |+| enum2)).run.unsafePerformIO must_===(true)
  }

  //checkAll(functor.laws[Enum])
  //checkAll(pointed.laws[Enum])
  //checkAll(monad.laws[Enum])

  object instances {
    //def functor[F[_] : Functor] = Functor[EnumeratorT[?, F]]
    def monad[F[_] : Monad]     = Monad[EnumeratorT[?, F]]
    def semigroup[E, F[_]: Bind] = Semigroup[EnumeratorT[E, F]]
    def monoid[E, F[_]: Monad] = Monoid[EnumeratorT[E, F]]
  }
}


// vim: set ts=4 sw=4 et:
