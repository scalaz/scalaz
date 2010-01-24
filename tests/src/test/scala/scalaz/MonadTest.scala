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
    type Z = Int


    implicit def IdentityEqual[X] = equalA[Identity[X]]
    checkMonadLaws[Identity, A]
    checkMonadLaws[List, A]
    // todo fix arbitrary instance for Stream
    //    checkMonadLaws[Stream, A]
    checkMonadLaws[NonEmptyList, A]

    implicit def StateEqual = implicitly[Equal[(Int, Unit)]] ∙ {s: State[Int, Unit] => s.apply(0)}
    implicit def StateArb: Arbitrary[State[Int, Unit]] = implicitly[Arbitrary[(Int => Int)]] ∘ (modify _)
    checkMonadLaws[PartialApply1Of2[State, A]#Apply, Unit]
    checkMonadLaws[PartialApply1Of2[Tuple2, B]#Apply, A]
    checkMonadLaws[PartialApply2Of3[Tuple3, B, C]#Apply, A]
    checkMonadLaws[PartialApply3Of4[Tuple4, B, C, D]#Apply, A]
    checkMonadLaws[PartialApply4Of5[Tuple5, B, C, D, E]#Apply, A]
    checkMonadLaws[PartialApply5Of6[Tuple6, B, C, D, E, F]#Apply, A]
    checkMonadLaws[PartialApply6Of7[Tuple7, B, C, D, E, F, G]#Apply, A]
    implicit def EqualFunction1 = implicitly[Equal[Int]] ∙ {f: (Int => Int) => f(0)}
    implicit def EqualFunction2 = implicitly[Equal[Int]] ∙ {f: ((Int, Int) => Int) => f(0, 0)}
    implicit def EqualFunction3 = implicitly[Equal[Int]] ∙ {f: ((Int, Int, Int) => Int) => f(0, 0, 0)}
    implicit def EqualFunction4 = implicitly[Equal[Int]] ∙ {f: ((Int, Int, Int, Int) => Int) => f(0, 0, 0, 0)}
    implicit def EqualFunction5 = implicitly[Equal[Int]] ∙ {f: ((Int, Int, Int, Int, Int) => Int) => f(0, 0, 0, 0, 0)}
//    implicit def EqualFunction6 = implicitly[Equal[Int]] ∙ {f: ((Int, Int, Int, Int, Int, Int) => Int) => f(0, 0, 0, 0, 0, 0)}
    checkMonadLaws[PartialApply1Of2[Function1, B]#Apply, A]
    checkMonadLaws[PartialApply2Of3[Function2, B, C]#Apply, A]
    checkMonadLaws[PartialApply3Of4[Function3, B, C, D]#Apply, A]
    checkMonadLaws[PartialApply4Of5[Function4, B, C, D, E]#Apply, A]
    checkMonadLaws[PartialApply5Of6[Function5, B, C, D, E, F]#Apply, A]
//    checkMonadLaws[PartialApply6Of7[Function6, B, C, D, E, F, G]#Apply, A]
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
