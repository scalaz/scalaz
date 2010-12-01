package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import org.scalacheck.Arbitrary
import scalacheck.{ScalazProperties, ScalazArbitrary, ScalaCheckBinding}

class FunctorTest extends Specification with Sugar with ScalaCheck {
  import Scalaz._
  import ScalaCheckBinding._
  import ScalazArbitrary._

  "functor laws" should {
    type A = Int
    type B = Int
    type R = Int
    type S = Int
    type T = Int
    type U = Int
    type V = Int
    type W = Int
    type X = Int
    type M = Int
    type P = Int

    implicit def IdentityEqual[X] = equalA[Identity[X]]
    checkFunctorLaws[Identity, A]
    checkFunctorLaws[NonEmptyList, A]
    checkFunctorLaws[ZipStream, A]
    checkFunctorLaws[Tuple1, A]
    checkFunctorLaws[({type λ[α]=(R, α)})#λ, A]
    checkFunctorLaws[({type λ[α]=(R, S, α)})#λ, A]
    checkFunctorLaws[({type λ[α]=(R, S, T, α)})#λ, A]
    checkFunctorLaws[({type λ[α]=(R, S, T, U, α)})#λ, A]
    checkFunctorLaws[({type λ[α]=(R, S, T, U, V, α)})#λ, A]

    // todo
    //    checkFunctorLaws[Function0, A]
    //    checkFunctorLaws[({type λ[α]=Function1[R, α]})#λ, A]
    //    checkFunctorLaws[({type λ[α]=Function2[R, S, α]})#λ, A]
    //    checkFunctorLaws[({type λ[α]=Function3[R, S, T, α]})#λ, A]
    //    checkFunctorLaws[({type λ[α]=Function4[R, S, T, U, α]})#λ, A]
    //    checkFunctorLaws[({type λ[α]=Function5[R, S, T, U, V, α]})#λ, A]
    //    checkFunctorLaws[({type λ[α]=Function6[R, S, T, U, V, W, α]})#λ, A]

    checkFunctorLaws[List, A]

    // todo fix arbitrary instance for Stream
    //    checkFunctorLaws[Stream, A]

    checkFunctorLaws[NonEmptyList, A]
    checkFunctorLaws[Option, A]
    checkFunctorLaws[FirstOption, A]
    checkFunctorLaws[LastOption, A]
    checkFunctorLaws[ArraySeq, A]
    checkFunctorLaws[({type λ[α]=Either.LeftProjection[α, X]})#λ, A]
    checkFunctorLaws[({type λ[α]=Either.RightProjection[X, α]})#λ, A]

    // todo
    //    checkFunctorLaws[Responder, A]
    //    checkFunctorLaws[PartialApplyKA[Kleisli, M, P]#Apply, A]

    import java.util.concurrent.Callable
    checkFunctorLaws[Callable, A]

    // todo
    import java.util.Map.Entry
    import java.util.AbstractMap.SimpleImmutableEntry

    //    checkFunctorLaws[({type λ[α]=Entry[X, α]})#λ, A]
    checkFunctorLaws[({type λ[α]=Validation[X, α]})#λ, A]
    checkFunctorLaws[({type λ[α]=FailProjection[α, X]})#λ, A]
    checkFunctorLaws[Zipper, A]

    // todo
    //    checkFunctorLaws[Tree, A]
    //    checkFunctorLaws[TreeLoc, A]
    //    import scalaz.concurrent.Promise
    //    implicit val strat = concurrent.strategy.Sequential.strategy[A]
    //    checkFunctorLaws[Promise, A]

    import java.util._
    import java.util.concurrent._
    checkFunctorLaws[ArrayList, A]

    // todo
    //    checkFunctorLaws[LinkedList, A]
    //    checkFunctorLaws[PriorityQueue, A]
    //    checkFunctorLaws[Stack, A]
    //    checkFunctorLaws[Vector, A]
    //    checkFunctorLaws[ArrayBlockingQueue, A]
    //    checkFunctorLaws[ConcurrentLinkedQueue, A]
    //    checkFunctorLaws[CopyOnWriteArrayList, A]
    //    checkFunctorLaws[LinkedBlockingQueue, A]
    //    checkFunctorLaws[SynchronousQueue, A]
  }

  def checkFunctorLaws[F[_], A](implicit mm: Functor[F],
                                ea: Equal[A],
                                man: Manifest[F[A]],
                                ema: Equal[F[A]],
                                arbma: Arbitrary[F[A]],
                                arba: Arbitrary[A]): Unit = {
    val typeName = man.toString
    typeName in {
      import ScalazProperties.Functor._
      identity[F, A] must pass
      associative[F, A, A, A] must pass
    }
  }
}
