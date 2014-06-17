package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop.forAll

object LazyEitherTTest extends SpecLite {

  import LazyEitherTest.LazyEitherEqual

  implicit def lazyEitherTEqual[F[_], A, B](implicit F0: Equal[F[LazyEither[A, B]]]): Equal[LazyEitherT[F, A, B]] =
    F0.contramap((_: LazyEitherT[F, A, B]).run)

  type LazyEitherTList[A, B] = LazyEitherT[List, A, B]
  type LazyEitherTListInt[A] = LazyEitherT[List, Int, A]

  checkAll(equal.laws[LazyEitherTListInt[Int]])
  checkAll(monad.laws[LazyEitherTListInt])
  checkAll(traverse.laws[LazyEitherTListInt])
  checkAll(bitraverse.laws[LazyEitherTList])
  checkAll(monadError.laws[LazyEitherTList, Int])

  object instances {
    def functor[F[_] : Functor, A] = Functor[({type λ[α] = LazyEitherT[F, A, α]})#λ]
    def monad[F[_] : Monad, A] = Monad[({type λ[α] = LazyEitherT[F, A, α]})#λ]
    def foldable[F[_] : Foldable, A] = Foldable[({type λ[α] = LazyEitherT[F, A, α]})#λ]
    def traverse[F[_] : Traverse, A] = Traverse[({type λ[α] = LazyEitherT[F, A, α]})#λ]
    def bifunctor[F[_] : Functor] = Bifunctor[({type λ[α, β] = LazyEitherT[F, α, β]})#λ]
    def bifoldable[F[_] : Foldable] = Bifoldable[({type λ[α, β] = LazyEitherT[F, α, β]})#λ]
    def bitraverse[F[_] : Traverse] = Bitraverse[({type λ[α, β] = LazyEitherT[F, α, β]})#λ]

    // checking absence of ambiguity
    def functor[F[_] : Monad : Traverse, A] = Functor[({type λ[α] = LazyEitherT[F, A, α]})#λ]
    def foldable[F[_] : Traverse, A] = Foldable[({type λ[α] = LazyEitherT[F, A, α]})#λ]
    def bifunctor[F[_] : Traverse] = Bifunctor[({type λ[α, β] = LazyEitherT[F, α, β]})#λ]
    def bifoldable[F[_] : Traverse] = Bifoldable[({type λ[α, β] = LazyEitherT[F, α, β]})#λ]
    def monadError[F[_] : Monad, A] = MonadError[({type λ[α, β] = LazyEitherT[F, α, β]})#λ, A]
  }

}
