package scalaz

import org.scalacheck.Arbitrary
import java.math.BigInteger
import scalacheck.{ScalazProperties, ScalazArbitrary, ScalaCheckBinding}
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

class MonadTest extends Specification with ScalaCheck {
  import Scalaz._
  import ScalaCheckBinding._
  import ScalazArbitrary._
  import State.State
  import std.either._
  import std.tuple._
  import std.function._
  import State.stateMonad


  "monad laws" should {
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
    checkMonadLaws[Identity, A]("Identity")
    checkMonadLaws[List, A]("List")
    // todo fix arbitrary instance for Stream
    //    checkMonadLaws[Stream, A]
    checkMonadLaws[NonEmptyList, A]("NonEmptyList")

    implicit def StateEqual: Equal[State[Int, Int]] = new Equal[State[Int, Int]] {
      def equal(a1: State.State[Int, Int], a2: State.State[Int, Int]) = a1.apply(0) == a2.apply(0)
    }
    implicit def StateArb: Arbitrary[State[Int, Int]] = Arbitrary(implicitly[Arbitrary[Function1[Int,Int]]].arbitrary.map(modify[Int]))
    checkMonadLaws[({type λ[α]=State[Int, α]})#λ, Int]("State")
    checkMonadLaws[Tuple1, A]("Tuple1")
    checkMonadLaws[({type λ[α]=(B, α)})#λ, A]("Tuple2")
    checkMonadLaws[({type λ[α]=(B, C, α)})#λ, A]("Tuple3")
    checkMonadLaws[({type λ[α]=(B, C, D, α)})#λ, A]("Tuple4")
    checkMonadLaws[({type λ[α]=(B, C, D, E, α)})#λ, A]("Tuple5")
    checkMonadLaws[({type λ[α]=(B, C, D, E, F, α)})#λ, A]("Tuple6")
    checkMonadLaws[({type λ[α]=(B, C, D, E, F, G, α)})#λ, A]("Tuple7")
    checkMonadLaws[({type λ[α]=(B, C, D, E, F, G, H, α)})#λ, A]("Tuple8")
    implicit def EqualFunction1 = Equal.equalBy[Int => Int, Int](_.apply(0))
    implicit def EqualFunction2 = Equal.equalBy[(Int, Int) => Int, Int](_.apply(0, 0))
    implicit def EqualFunction3 = Equal.equalBy[(Int, Int, Int) => Int, Int](_.apply(0, 0, 0))
    implicit def EqualFunction4 = Equal.equalBy[(Int, Int, Int, Int) => Int, Int](_.apply(0, 0, 0, 0))
    implicit def EqualFunction5 = Equal.equalBy[(Int, Int, Int, Int, Int) => Int, Int](_.apply(0, 0, 0, 0, 0))
//    implicit def EqualFunction6 = implicitly[Equal[Int]] ∙ {f: ((Int, Int, Int, Int, Int, Int) => Int) => f(0, 0, 0, 0, 0, 0)}
    checkMonadLaws[({type λ[α]=(B) => α})#λ, A]("Function1")
    checkMonadLaws[({type λ[α]=(B, C) => α})#λ, A]("Function2")
    checkMonadLaws[({type λ[α]=(B, C, D) => α})#λ, A]("Function3")
    checkMonadLaws[({type λ[α]=(B, C, D, E) => α})#λ, A]("Function4")
    checkMonadLaws[({type λ[α]=(B, C, D, E, F) => α})#λ, A]("Function5")
//    checkMonadLaws[({type λ[α]=Function6[B, C, D, E, F, G, α]})#λ, A]
    checkMonadLaws[({type λ[α]=Either.LeftProjection[α, X]})#λ, A]("Either.LeftProjection")
    checkMonadLaws[({type λ[α]=Either.RightProjection[X, α]})#λ, A]("Either.RightProjection")
//    checkMonadLaws[({type λ[α]=Entry[X, α]})#λ, A]
  }

  def checkMonadLaws[M[_], A](typeName: String)(implicit mm: Monad[M],
                              ea: Equal[A],
                              ema: Equal[M[A]],
                              arbma: Arbitrary[M[A]],
                              arba: Arbitrary[A]
          ) = {
    typeName in {
      import ScalazProperties.monad._
      check(leftIdentity[M, A, A])
      check(rightIdentity[M, A])
      check(associativity[M, A, A, A])
    }
  }
}
