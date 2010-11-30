package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import org.scalacheck.Arbitrary
import java.math.BigInteger
import scalacheck.{ScalazProperties, ScalazArbitrary, ScalaCheckBinding}

class MonadTest extends Specification with Sugar with ScalaCheck {
  import Scalaz._
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


    implicit def IdentityEqual[X] = equalA[Identity[X]]
    checkMonadLaws[Identity, A]
    checkMonadLaws[List, A]
    // todo fix arbitrary instance for Stream
    //    checkMonadLaws[Stream, A]
    checkMonadLaws[NonEmptyList, A]

    implicit def StateEqual = implicitly[Equal[(Int, Unit)]] ∙ {s: State[Int, Unit] => s.apply(0)}
    implicit def StateArb: Arbitrary[State[Int, Unit]] = implicitly[Arbitrary[(Int => Int)]] ∘ (modify _)
    checkMonadLaws[({type λ[α]=State[A, α]})#λ, Unit]
    checkMonadLaws[({type λ[α]=(B, α)})#λ, A]
    checkMonadLaws[({type λ[α]=(B, C, α)})#λ, A]
    checkMonadLaws[({type λ[α]=(B, C, D, α)})#λ, A]
    checkMonadLaws[({type λ[α]=(B, C, D, E, α)})#λ, A]
    checkMonadLaws[({type λ[α]=(B, C, D, E, F, α)})#λ, A]
    checkMonadLaws[({type λ[α]=(B, C, D, E, F, G, α)})#λ, A]
    implicit def EqualFunction1 = implicitly[Equal[Int]] ∙ {f: (Int => Int) => f(0)}
    implicit def EqualFunction2 = implicitly[Equal[Int]] ∙ {f: ((Int, Int) => Int) => f(0, 0)}
    implicit def EqualFunction3 = implicitly[Equal[Int]] ∙ {f: ((Int, Int, Int) => Int) => f(0, 0, 0)}
    implicit def EqualFunction4 = implicitly[Equal[Int]] ∙ {f: ((Int, Int, Int, Int) => Int) => f(0, 0, 0, 0)}
    implicit def EqualFunction5 = implicitly[Equal[Int]] ∙ {f: ((Int, Int, Int, Int, Int) => Int) => f(0, 0, 0, 0, 0)}
//    implicit def EqualFunction6 = implicitly[Equal[Int]] ∙ {f: ((Int, Int, Int, Int, Int, Int) => Int) => f(0, 0, 0, 0, 0, 0)}
    checkMonadLaws[({type λ[α]=(B) => α})#λ, A]
    checkMonadLaws[({type λ[α]=(B, C) => α})#λ, A]
    checkMonadLaws[({type λ[α]=(B, C, D) => α})#λ, A]
    checkMonadLaws[({type λ[α]=(B, C, D, E) => α})#λ, A]
    checkMonadLaws[({type λ[α]=(B, C, D, E, F) => α})#λ, A]
//    checkMonadLaws[({type λ[α]=Function6[B, C, D, E, F, G, α]})#λ, A]
    checkMonadLaws[({type λ[α]=Either.LeftProjection[α, X]})#λ, A]
    checkMonadLaws[({type λ[α]=Either.RightProjection[X, α]})#λ, A]
//    checkMonadLaws[({type λ[α]=Entry[X, α]})#λ, A]
    checkMonadLaws[({type λ[α]=Validation[X, α]})#λ, A]
    checkMonadLaws[({type λ[α]=FailProjection[α, X]})#λ, A]
    ()
  }

  def checkMonadLaws[M[_], A](implicit mm: Monad[M],
                              ea: Equal[A],
                              man: Manifest[M[A]],
                              ema: Equal[M[A]],
                              arbma: Arbitrary[M[A]],
                              arba: Arbitrary[A]
          ): Unit = {
    val typeName = man.toString
    typeName in {
      import ScalazProperties.Monad._
      identity[M, A] must pass
      unit[M, A, A] must pass
      composition[M, A, A, A] must pass
    }
  }
}
