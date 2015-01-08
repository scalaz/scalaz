package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop.forAll

object LazyOptionTTest extends SpecLite {

  type LazyOptionTList[A] = LazyOptionT[List, A]

  checkAll(equal.laws[LazyOptionTList[Int]])
  checkAll(monad.laws[LazyOptionTList])

  object instances {
    def functor[F[_] : Functor] = Functor[LazyOptionT[F, ?]]
    def monad[F[_] : Monad] = Monad[LazyOptionT[F, ?]]

    // checking absence of ambiguity
    def functor[F[_] : Monad] = Functor[LazyOptionT[F, ?]]
    def apply[F[_] : Monad] = Apply[LazyOptionT[F, ?]]
  }
}
