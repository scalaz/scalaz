package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

object LazyEitherTTest extends SpecLite {

  import LazyEitherTest.LazyEitherEqual

  implicit def lazyEitherTEqual[F[_], A, B](implicit F0: Equal[F[LazyEither[A, B]]]): Equal[LazyEitherT[F, A, B]] =
    F0.contramap((_: LazyEitherT[F, A, B]).run)

  type LazyEitherTList[A, B] = LazyEitherT[List, A, B]
  type LazyEitherTListInt[A] = LazyEitherT[List, Int, A]

  checkAll(equal.laws[LazyEitherTListInt[Int]])
  checkAll(monadPlus.laws[LazyEitherTListInt])
  checkAll(traverse.laws[LazyEitherTListInt])
  checkAll(bitraverse.laws[LazyEitherTList])
  checkAll(monadError.laws[LazyEitherTList, Int])

  private def lazyEitherTUcompilationTest: Unit = {
    val a: String \/ LazyEither[Int, Boolean] = null
    LazyEitherT.lazyEitherTU(a)
  }

  object instances {
    def functor[F[_] : Functor, A] = Functor[LazyEitherT[F, A, ?]]
    def plus[F[_] : Monad, A: Semigroup] = Plus[LazyEitherT[F, A, ?]]
    def monad[F[_] : Monad, A] = Monad[LazyEitherT[F, A, ?]]
    def monadPlus[F[_] : Monad, A: Monoid] = MonadPlus[LazyEitherT[F, A, ?]]
    def foldable[F[_] : Foldable, A] = Foldable[LazyEitherT[F, A, ?]]
    def traverse[F[_] : Traverse, A] = Traverse[LazyEitherT[F, A, ?]]
    def bifunctor[F[_] : Functor] = Bifunctor[LazyEitherT[F, ?, ?]]
    def bifoldable[F[_] : Foldable] = Bifoldable[LazyEitherT[F, ?, ?]]
    def bitraverse[F[_] : Traverse] = Bitraverse[LazyEitherT[F, ?, ?]]

    // checking absence of ambiguity
    def functor[F[_] : Monad : Traverse, A] = Functor[LazyEitherT[F, A, ?]]
    def functor[F[_] : Monad, A: Monoid] = Functor[LazyEitherT[F, A, ?]]
    def monad[F[_] : Monad, A: Monoid] = Monad[LazyEitherT[F, A, ?]]
    def foldable[F[_] : Traverse, A] = Foldable[LazyEitherT[F, A, ?]]
    def bifunctor[F[_] : Traverse] = Bifunctor[LazyEitherT[F, ?, ?]]
    def bifoldable[F[_] : Traverse] = Bifoldable[LazyEitherT[F, ?, ?]]
    def monadError[F[_] : Monad, A] = MonadError[LazyEitherT[F, ?, ?], A]
  }

}
