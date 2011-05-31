package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import org.scalacheck.Arbitrary
import java.math.BigInteger
import scalacheck.{ScalazProperty, ScalazArbitrary, ScalaCheckBinding}

class MonadTest extends Specification with Sugar with ScalaCheck {

  import Scalaz._
  import data._
  import ScalaCheckBinding._
  import ScalazArbitrary._


  "monad laws" should {
    type A = Int
    type B = Int
    type C = Int
    type D = Int
    type E = Int
    type F = Int
    type G = Int
    type R = Int
    type X = Int
    type Z = Int


    checkMonadLaws[Ident, A]("Identity")
    checkMonadLaws[List, A]("List")
    // todo fix arbitrary instance for Stream
    //    checkMonadLaws[Stream, A]
    checkMonadLaws[NonEmptyList, A]("NonEmptyList")

    implicit def StateEqual = implicitly[Equal[(Int, Unit)]] ∙ {
      s: State[Int, Unit] => {
        val (i, j) = s.run.apply(0)
        (j, i)
      }
    }
    implicit def StateArb: Arbitrary[State[Int, Unit]] = implicitly[Arbitrary[(Int => Int)]] ∘ (modify _)
    checkMonadLaws[({type λ[α] = State[A, α]})#λ, Unit]("State")
    checkMonadLaws[({type λ[α] = (B, α)})#λ, A]("Tuple1")
    checkMonadLaws[({type λ[α] = (B, C, α)})#λ, A]("Tuple2")
    checkMonadLaws[({type λ[α] = (B, C, D, α)})#λ, A]("Tuple3")
    checkMonadLaws[({type λ[α] = (B, C, D, E, α)})#λ, A]("Tuple4")
    checkMonadLaws[({type λ[α] = (B, C, D, E, F, α)})#λ, A]("Tuple5")
    checkMonadLaws[({type λ[α] = (B, C, D, E, F, G, α)})#λ, A]("Tuple6")
    implicit def EqualFunction1 = implicitly[Equal[Int]] ∙ {
      f: (Int => Int) => f(0)
    }
    implicit def EqualFunction2 = implicitly[Equal[Int]] ∙ {
      f: ((Int, Int) => Int) => f(0, 0)
    }
    implicit def EqualFunction3 = implicitly[Equal[Int]] ∙ {
      f: ((Int, Int, Int) => Int) => f(0, 0, 0)
    }
    implicit def EqualFunction4 = implicitly[Equal[Int]] ∙ {
      f: ((Int, Int, Int, Int) => Int) => f(0, 0, 0, 0)
    }
    implicit def EqualFunction5 = implicitly[Equal[Int]] ∙ {
      f: ((Int, Int, Int, Int, Int) => Int) => f(0, 0, 0, 0, 0)
    }
    //    implicit def EqualFunction6 = implicitly[Equal[Int]] ∙ {f: ((Int, Int, Int, Int, Int, Int) => Int) => f(0, 0, 0, 0, 0, 0)}
    checkMonadLaws[({type λ[α] = (B) => α})#λ, A]("Function1")
    checkMonadLaws[({type λ[α] = (B, C) => α})#λ, A]("Function2")
    checkMonadLaws[({type λ[α] = (B, C, D) => α})#λ, A]("Function3")
    checkMonadLaws[({type λ[α] = (B, C, D, E) => α})#λ, A]("Function4")
    checkMonadLaws[({type λ[α] = (B, C, D, E, F) => α})#λ, A]("Function5")
    //    checkMonadLaws[({type λ[α]=Function6[B, C, D, E, F, G, α]})#λ, A]
    checkMonadLaws[({type λ[α] = Either.LeftProjection[α, X]})#λ, A]("Either.left")
    checkMonadLaws[({type λ[α] = Either.RightProjection[X, α]})#λ, A]("Either.right")
    //    checkMonadLaws[({type λ[α]=Entry[X, α]})#λ, A]
    ()
  }

  def checkMonadLaws[M[_], A](n: String)(implicit mm: Monad[M],
                                          ea: Equal[A],
                                          ema: Equal[M[A]],
                                          arbma: Arbitrary[M[A]],
                                          arba: Arbitrary[A]
                                        ): Unit =
    n in {
      import ScalazProperty.Monad._
      leftIdentity[M, A, A] must pass
      rightIdentity[M, A] must pass
      associativity[M, A, A, A] must pass
    }
}
