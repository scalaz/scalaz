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
    checkFunctorLaws[PartialApply1Of2[Tuple2, R]#Apply, A]
    checkFunctorLaws[PartialApply2Of3[Tuple3, R, S]#Apply, A]
    checkFunctorLaws[PartialApply3Of4[Tuple4, R, S, T]#Apply, A]
    checkFunctorLaws[PartialApply4Of5[Tuple5, R, S, T, U]#Apply, A]
    checkFunctorLaws[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply, A]

    // todo
    //    checkFunctorLaws[Function0, A]
    //    checkFunctorLaws[PartialApply1Of2[Function1, R]#Apply, A]
    //    checkFunctorLaws[PartialApply2Of3[Function2, R, S]#Apply, A]
    //    checkFunctorLaws[PartialApply3Of4[Function3, R, S, T]#Apply, A]
    //    checkFunctorLaws[PartialApply4Of5[Function4, R, S, T, U]#Apply, A]
    //    checkFunctorLaws[PartialApply5Of6[Function5, R, S, T, U, V]#Apply, A]
    //    checkFunctorLaws[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply, A]

    checkFunctorLaws[List, A]

    // todo fix arbitrary instance for Stream
    //    checkFunctorLaws[Stream, A]

    checkFunctorLaws[NonEmptyList, A]
    checkFunctorLaws[Option, A]
    checkFunctorLaws[FirstOption, A]
    checkFunctorLaws[LastOption, A]
    checkFunctorLaws[GArray, A]
    checkFunctorLaws[PartialApply1Of2[Either.LeftProjection, X]#Flip, A]
    checkFunctorLaws[PartialApply1Of2[Either.RightProjection, X]#Apply, A]

    // todo
    //    checkFunctorLaws[Responder, A]
    //    checkFunctorLaws[PartialApplyKA[Kleisli, M, P]#Apply, A]

    import java.util.concurrent.Callable
    checkFunctorLaws[Callable, A]

    // todo
    import java.util.Map.Entry
    import java.util.AbstractMap.SimpleImmutableEntry

    //    checkFunctorLaws[PartialApply1Of2[Entry, X]#Apply, A]
    checkFunctorLaws[PartialApply1Of2[Validation, X]#Apply, A]
    checkFunctorLaws[PartialApply1Of2[FailProjection, X]#Flip, A]
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
