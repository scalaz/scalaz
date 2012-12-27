package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class StoreTTest extends Spec {

  implicit def storeTuple1IntEqual = new Equal[StoreT[Tuple1, Int, Int]] {
    def equal(a1: StoreT[Tuple1, Int, Int], a2: StoreT[Tuple1, Int, Int]) = (a1.run, a2.run) match {
      case ((tf1, x1), (tf2, x2)) => Equal[Int].equal(x1, x2) && Equal[Int].equal(tf1._1(0), tf2._1(0))
    }
  }

  checkAll(comonad.laws[({type λ[α] = StoreT[Tuple1, Int, α]})#λ])

  object instances {
    type A = Int
    def functor[F[+_] : Functor] = Functor[({type λ[+α] = StoreT[F, A, α]})#λ]
    def cobind[F[+_] : Cobind] = Cobind[({type λ[+α] = StoreT[F, A, α]})#λ]
    def comonad[F[+_] : Comonad] = Comonad[({type λ[+α] = StoreT[F, A, α]})#λ]

    // checking absence of ambiguity
    def functor[F[+_] : Comonad] = Functor[({type λ[+α] = StoreT[F, A, α]})#λ]
    def cobind[F[+_] : Comonad] = Cobind[({type λ[+α] = StoreT[F, A, α]})#λ]
  }

}
