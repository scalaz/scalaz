package scalaz

import std.AllInstances._
import Tags._
import scalacheck.{ScalazProperties, ScalazArbitrary}
import ScalazArbitrary._
import ScalazProperties.{monad, monadPlus, plus}

class MonadTest extends Spec {
  type A = Int
  type B = Int
  type C = Int
  type D = Int
  type E = Int
  type F = Int
  type G = Int
  type H = Int
  type R = Int
  type X = Int
  type Z = Int

  implicit def IdentityEqual[X: Equal] = Equal.equalBy[Identity[X], X](_.value)
  checkAll("Id", monad.laws[Id])
  checkAll("Value", monad.laws[Value])
  checkAll("Name", monad.laws[Name])
  checkAll("Need", monad.laws[Need])
  checkAll("Option", monadPlus.laws[Option])
  checkAll("Option @@ First", monad.laws[({type f[x] = Option[x] @@ First})#f])
  checkAll("Option @@ Last", monad.laws[({type f[x] = Option[x] @@ Last})#f])
  checkAll("List", monadPlus.laws[List])
  checkAll("Stream", monadPlus.laws[Stream])
  checkAll("NonEmptyList", monad.laws[NonEmptyList])
  checkAll("NonEmptyList", plus.laws[NonEmptyList])

  implicit def StateEqual: Equal[State[Int, Int]] = new Equal[State[Int, Int]] {
    def equal(a1: State[Int, Int], a2: State[Int, Int]) = a1.apply(0) == a2.apply(0)
  }
//  implicit def StateArb: Arbitrary[State[S, A]] = arbitrary[S => (A, S)].map(State(_))
//  checkAll("State", monad.laws[({type λ[α]=State[Int, α]})#λ])
  checkAll("Tuple1", monad.laws[Tuple1])
  checkAll("Tuple2", monad.laws[({type λ[α] = (B, α)})#λ])
  checkAll("Tuple3", monad.laws[({type λ[α] = (B, C, α)})#λ])
  checkAll("Tuple4", monad.laws[({type λ[α] = (B, C, D, α)})#λ])
  checkAll("Tuple5", monad.laws[({type λ[α] = (B, C, D, E, α)})#λ])
  checkAll("Tuple6", monad.laws[({type λ[α] = (B, C, D, E, F, α)})#λ])
  checkAll("Tuple7", monad.laws[({type λ[α] = (B, C, D, E, F, G, α)})#λ])
  checkAll("Tuple8", monad.laws[({type λ[α] = (B, C, D, E, F, G, H, α)})#λ])
  implicit def EqualFunction0 = Equal.equalBy[() => Int, Int](_.apply())
  implicit def EqualFunction1 = Equal.equalBy[Int => Int, Int](_.apply(0))
  implicit def EqualFunction2 = Equal.equalBy[(Int, Int) => Int, Int](_.apply(0, 0))
  implicit def EqualFunction3 = Equal.equalBy[(Int, Int, Int) => Int, Int](_.apply(0, 0, 0))
  implicit def EqualFunction4 = Equal.equalBy[(Int, Int, Int, Int) => Int, Int](_.apply(0, 0, 0, 0))
  implicit def EqualFunction5 = Equal.equalBy[(Int, Int, Int, Int, Int) => Int, Int](_.apply(0, 0, 0, 0, 0))
  checkAll("Function0", monad.laws[Function0])
  checkAll("Function1", monad.laws[({type λ[α] = (B) => α})#λ])
  checkAll("Function2", monad.laws[({type λ[α] = (B, C) => α})#λ])
  checkAll("Function3", monad.laws[({type λ[α] = (B, C, D) => α})#λ])
  checkAll("Function4", monad.laws[({type λ[α] = (B, C, D, E) => α})#λ])
  checkAll("Function5", monad.laws[({type λ[α] = (B, C, D, E, F) => α})#λ])
  checkAll("Either.LeftProjection", monad.laws[({type λ[α] = Either.LeftProjection[α, X]})#λ])
  checkAll("Either.RightProjection", monad.laws[({type λ[α] = Either.RightProjection[X, α]})#λ])
  checkAll("Either.LeftProjection @@ First", monad.laws[({type λ[α] = Either.LeftProjection[α, X] @@ First})#λ])
  checkAll("Either.RightProjection @@ First", monad.laws[({type λ[α] = Either.RightProjection[X, α] @@ First})#λ] )
  checkAll("Either.LeftProjection @@ Last", monad.laws[({type λ[α] = Either.LeftProjection[α, X] @@ Last})#λ])
  checkAll("Either.RightProjection @@ Last", monad.laws[({type λ[α] = Either.RightProjection[X, α] @@ Last})#λ])
}
