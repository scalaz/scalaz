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

class EnumeratorTTest extends Spec {
  implicit def enumeratorTArb[F[_], A](implicit FA: Arbitrary[List[A]], F: Monad[F]): Arbitrary[EnumeratorT[Unit, A, F]] = Functor[Arbitrary].map(FA)(l => EnumeratorT.enumStream[Unit, A, F](l.toStream))

  implicit def enumEqual[A](implicit EQ: Equal[A]): Equal[Enum[A]] = new Equal[Enum[A]] {
    def equal(en1: Enum[A], en2: Enum[A]): Boolean = {
      val l1 = (consume[Unit, A, Id, List] &= en1).run(_ => Nil)
      val l2 = (consume[Unit, A, Id, List] &= en2).run(_ => Nil)
      l1 === l2
    }
  }

  "eof" in {
    val enum = enumEofT[Unit, Int, Id]
    (consume[Unit, Int, Id, List] &= enum).run(_ => List(1)) must be_===(Nil)
  }

  "map" in {
    val enum = enumStream[Unit, Int, Id](Stream(1, 2, 3))
    type EnumId[α] = EnumeratorT[Unit, α, Id]
    (consume[Unit, Int, Id, List] &= enum.map(_ * 2)).runOrZero must be_===(List(2, 4, 6))
  }
  
  "flatMap" in {
    val enum = enumStream[Unit, Int, Id](Stream(1, 2, 3))
    type EnumId[α] = EnumeratorT[Unit, α, Id]
    (consume[Unit, Int, Id, List] &= enum.flatMap(i => enum.map(_ + i))).runOrZero must be_===(List(2, 3, 4, 3, 4, 5, 4, 5, 6))
  }

  "uniq" in {
    val enum = enumStream[Unit, Int, Id](Stream(1, 1, 2, 2, 2, 3, 3))
    type EnumId[α] = EnumeratorT[Unit, α, Id]
    (consume[Unit, Int, Id, List] &= enum.uniq).runOrZero must be_===(List(1, 2, 3))
  }

  "zipWithIndex" in {
    val enum = enumStream[Unit, Int, Id](Stream(3, 4, 5))
    type EnumId[α] = EnumeratorT[Unit, α, Id]
    (consume[Unit, (Int, Long), Id, List] &= enum.zipWithIndex).runOrZero must be_===(List((3, 0L), (4, 1L), (5, 2L)))
  }

  "zipWithIndex" in {
    val enum = enumStream[Unit, Int, Id](Stream(3, 4, 5))
    type EnumId[α] = EnumeratorT[Unit, α, Id]
    (consume[Unit, (Int, Long), Id, List] &= enum.zipWithIndex).runOrZero must be_===(List((3, 0L), (4, 1L), (5, 2L)))
  }

  "zipWithIndex in combination with another function" in {
    val enum = enumStream[Unit, Int, Id](Stream(3, 4, 4, 5))
    type EnumId[α] = EnumeratorT[Unit, α, Id]
    (consume[Unit, (Int, Long), Id, List] &= enum.uniq.zipWithIndex).runOrZero must be_===(List((3, 0L), (4, 1L), (5, 2L)))
  }

  "lift" in {
    val enum = EnumeratorT.enumeratorTMonadTrans[Unit].liftM(List(1, 2, 3))
    (collectT[Unit, Int, List, Id] &= enum.map(_ * 2)).runOrZero must be_===(List(2, 4, 6))
  }

  "enumerate an array" in {
    val enum = enumArray[Unit, Int, Id](Array(1, 2, 3, 4, 5), 0, Some(3))
    (consume[Unit, Int, Id, List] &= enum).runOrZero must be_===(List(1, 2, 3))
  }

  "allow for nesting of monads" in {
    type OIO[α] = OptionT[IO, α]
    val enum = enumIterator[Unit, Int, OIO](List(1, 2, 3).iterator)
    (consume[Unit, Int, OIO, List] &= enum.map(_ * 2)).run(_ => sys.error("unexpected")).run.unsafePerformIO must be_===(Some(List(2, 4, 6)))
  }

  "drain" in {
    val enum = enumStream[Unit, Int, Id](Stream(1, 2, 3))
    enum.drainTo[List] must be_===(List(1, 2, 3))
  }

  //checkAll(functor.laws[Enum])
  //checkAll(pointed.laws[Enum])
  checkAll(monad.laws[Enum])
  
  object instances {
    //def functor[F[_] : Functor] = Functor[({type λ[α] = EnumeratorT[Unit, α, F]})#λ]
    //def pointed[F[_] : Pointed] = Pointed[({type λ[α] = EnumeratorT[Unit, α, F]})#λ]
    def monad[F[_] : Monad]     = Monad[({type λ[α] = EnumeratorT[Unit, α, F]})#λ]
    def semigroup[X, E, F[_]: Bind] = Semigroup[EnumeratorT[X, E, F]]
    def monoid[X, E, F[_]: Monad] = Monoid[EnumeratorT[X, E, F]]
  }
}


// vim: set ts=4 sw=4 et:
