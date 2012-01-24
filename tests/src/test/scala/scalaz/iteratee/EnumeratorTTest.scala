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
    (consume[Unit, Int, Id, List] &= enum.map(_ * 2)).run(_ => List.empty[Int]) must be_===(List(2, 4, 6))
  }
  
  "flatMap" in {
    val enum = enumStream[Unit, Int, Id](Stream(1, 2, 3))
    type EnumId[α] = EnumeratorT[Unit, α, Id]
    (consume[Unit, Int, Id, List] &= enum.flatMap(i => enum.map(_ + i))).run(_ => List.empty[Int]) must be_===(List(2, 3, 4, 3, 4, 5, 4, 5, 6))
  }

  //checkAll(functor.laws[Enum])
  //checkAll(pointed.laws[Enum])
  checkAll(monad.laws[Enum])
  
  object instances {
    //def functor[F[_] : Functor] = Functor[({type λ[α] = EnumeratorT[Unit, α, F]})#λ]
    //def pointed[F[_] : Pointed] = Pointed[({type λ[α] = EnumeratorT[Unit, α, F]})#λ]
    def monad[F[_] : Monad]     = Monad[({type λ[α] = EnumeratorT[Unit, α, F]})#λ]
  }
}


// vim: set ts=4 sw=4 et:
