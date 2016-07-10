package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

object LazyOptionTTest extends SpecLite {

  type LazyOptionTList[A] = LazyOptionT[List, A]

  checkAll(equal.laws[LazyOptionTList[Int]])
  checkAll(monadPlus.laws[LazyOptionTList])
  checkAll(bindRec.laws[LazyOptionTList])

  "tail recursive tailrecM" in {
    import Scalaz.Id
    type LazyOptionId[A] = LazyOptionT[Id, A]

    val times = 10000

    val result =
      BindRec[LazyOptionId].tailrecM[Int, Int] {
        i => LazyOptionT[Id, Int \/ Int](LazyOption.lazySome(if (i < 10000) \/.left(i + 1) else \/.right(i)))
      }(0)
    result.getOrElse(0) must_=== times
  }

  object instances {
    def functor[F[_] : Functor] = Functor[LazyOptionT[F, ?]]
    def monad[F[_] : Monad] = Monad[LazyOptionT[F, ?]]
    def bindRec[F[_] : Monad: BindRec] = BindRec[LazyOptionT[F, ?]]

    // checking absence of ambiguity
    def functor[F[_] : Monad] = Functor[LazyOptionT[F, ?]]
    def functor[F[_] : Monad: BindRec] = Functor[LazyOptionT[F, ?]]
    def apply[F[_] : Monad] = Apply[LazyOptionT[F, ?]]
    def bind[F[_] : Monad: BindRec] = Bind[LazyOptionT[F, ?]]
  }
}
