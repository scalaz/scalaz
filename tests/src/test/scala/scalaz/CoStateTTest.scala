package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class CoStateTTest extends Spec {

  implicit def coStateTuple1IntEqual = new Equal[CoStateT[Tuple1, Int, Int]] {
    def equal(a1: CoStateT[Tuple1, Int, Int], a2: CoStateT[Tuple1, Int, Int]) = (a1.run, a2.run) match {
      case ((tf1, x1), (tf2, x2)) => Equal[Int].equal(x1, x2) && Equal[Int].equal(tf1._1(0), tf2._1(0))
    }
  }

  checkAll(comonad.laws[({type λ[α] = CoStateT[Tuple1, Int, α]})#λ])

  object instances {
    type A = Int
    def functor[F[_] : Functor] = Functor[({type λ[α] = CoStateT[F, A, α]})#λ]
    def copointed[F[_] : CoPointed] = CoPointed[({type λ[α] = CoStateT[F, A, α]})#λ]
    def cobind[F[_] : CoBind] = CoBind[({type λ[α] = CoStateT[F, A, α]})#λ]
    def comonad[F[_] : CoMonad] = CoMonad[({type λ[α] = CoStateT[F, A, α]})#λ]

    // checking absence of ambiguity
    def functor[F[_] : CoMonad] = Functor[({type λ[α] = CoStateT[F, A, α]})#λ]
    def copointed[F[_] : CoMonad] = CoPointed[({type λ[α] = CoStateT[F, A, α]})#λ]
    def cobind[F[_] : CoMonad] = CoBind[({type λ[α] = CoStateT[F, A, α]})#λ]
  }

}
