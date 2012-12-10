package scalaz

import scalaz.testlib.ScalazProperties._
import scalaz.testlib.ScalazArbitrary._
import std.AllInstances._

class LazyOptionTTest extends testlib.Spec {

  type LazyOptionTList[A] = LazyOptionT[List, A]

  checkAll(equal.laws[LazyOptionTList[Int]])
  checkAll(monad.laws[LazyOptionTList])

  object instances {
    def functor[F[+_] : Functor] = Functor[({type λ[α] = LazyOptionT[F, α]})#λ]
    def pointed[F[+_] : Pointed] = Pointed[({type λ[α] = LazyOptionT[F, α]})#λ]
    def apply[F[+_] : Apply] = Apply[({type λ[α] = LazyOptionT[F, α]})#λ]
    def monad[F[+_] : Monad] = Monad[({type λ[α] = LazyOptionT[F, α]})#λ]

    // checking absence of ambiguity
    def functor[F[+_] : Monad] = Functor[({type λ[α] = LazyOptionT[F, α]})#λ]
    def pointed[F[+_] : Monad] = Pointed[({type λ[α] = LazyOptionT[F, α]})#λ]
    def apply[F[+_] : Monad] = Apply[({type λ[α] = LazyOptionT[F, α]})#λ]
  }
}
