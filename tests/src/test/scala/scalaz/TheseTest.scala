package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import \&/._
import syntax.contravariant._

class TheseTest extends Spec {
  type TheseInt[a] = Int \&/ a

  checkAll(monad.laws[TheseInt])
  checkAll(cobind.laws[TheseInt])
  checkAll(traverse.laws[TheseInt])
  checkAll(equal.laws[Int \&/ Int])
  checkAll(bitraverse.laws[\&/])

  implicit def ephemeralStreamShow[A: Show]: Show[EphemeralStream[A]] =
    Show[List[A]].contramap(_.toList)

  "align unalign" should {
    "List" ! prop { (a: List[Int], b: List[Int]) =>
      unalignList(alignList(a, b)) must be_=== ((a, b))
    }
    "EphemeralStream" ! prop { (a: EphemeralStream[Int], b: EphemeralStream[Int]) =>
      unalignStream(alignStream(a, b)) must be_=== ((a, b))
    }
  }

  object instances {
    def functor[L] = Functor[({type λ[α] = L \&/ α})#λ]
    def apply[L: Semigroup] = Apply[({type λ[α] = L \&/ α})#λ]
    def applicative[L: Semigroup] = Applicative[({type λ[α] = L \&/ α})#λ]
    def monad[L: Semigroup] = Monad[({type λ[α] = L \&/ α})#λ]
    def cobind[L] = Cobind[({type λ[α] = L \&/ α})#λ]
    def foldable[L] = Foldable[({type λ[α] = L \&/ α})#λ]
    def traverse[L] = Traverse[({type λ[α] = L \&/ α})#λ]
    def bifunctor = Bifunctor[\&/]
    def bifoldable = Bifoldable[\&/]
    def bitraverse = Bitraverse[\&/]

    // checking absence of ambiguity
    def functor[L: Semigroup] = Functor[({type λ[α] = L \&/ α})#λ]
  }
}
