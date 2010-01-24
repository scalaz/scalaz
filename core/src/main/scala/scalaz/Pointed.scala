package scalaz

trait Pointed[P[_]] extends Functor[P] with Pure[P]

object Pointed {
  implicit def pointed[P[_]](implicit t: Functor[P], p: Pure[P]) = new Pointed[P] {
    def fmap[A, B](a: P[A], f: A => B) = t.fmap(a, f)
    def pure[A](a: => A): P[A] = p.pure(a)
  }
  
  import Functor._
  import Pure._
  
  implicit def StatePointed[A] = pointed[PartialApply1Of2[State, A]#Apply](StateFunctor, StatePure)

  implicit def Tuple2Pointed[R: Monoid]: Pointed[PartialApply1Of2[Tuple2, R]#Apply] = pointed[PartialApply1Of2[Tuple2, R]#Apply](Tuple2Functor, Tuple2Pure)

  implicit def Tuple3Pointed[R: Monoid, S: Monoid]: Pointed[PartialApply2Of3[Tuple3, R, S]#Apply] = pointed[PartialApply2Of3[Tuple3, R, S]#Apply](Tuple3Functor, Tuple3Pure)

  implicit def Tuple4Pointed[R: Monoid, S: Monoid, T: Monoid]: Pointed[PartialApply3Of4[Tuple4, R, S, T]#Apply] = pointed[PartialApply3Of4[Tuple4, R, S, T]#Apply](Tuple4Functor, Tuple4Pure)

  implicit def Tuple5Pointed[R: Monoid, S: Monoid, T: Monoid, U: Monoid]: Pointed[PartialApply4Of5[Tuple5, R, S, T, U]#Apply] = pointed[PartialApply4Of5[Tuple5, R, S, T, U]#Apply](Tuple5Functor, Tuple5Pure)

  implicit def Tuple6Pointed[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid]: Pointed[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply] = pointed[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply](Tuple6Functor, Tuple6Pure)

  implicit def Tuple7Pointed[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid, W: Monoid]: Pointed[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply] = pointed[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply](Tuple7Functor, Tuple7Pure)
  
  implicit def Function1Pointed[R]: Pointed[PartialApply1Of2[Function1, R]#Apply] = pointed[PartialApply1Of2[Function1, R]#Apply](Function1Functor, Function1Pure)

  implicit def Function2Pointed[R, S]: Pointed[PartialApply2Of3[Function2, R, S]#Apply] = pointed[PartialApply2Of3[Function2, R, S]#Apply](Function2Functor, Function2Pure)

  implicit def Function3Pointed[R, S, T]: Pointed[PartialApply3Of4[Function3, R, S, T]#Apply] = pointed[PartialApply3Of4[Function3, R, S, T]#Apply](Function3Functor, Function3Pure)

  implicit def Function4Pointed[R, S, T, U]: Pointed[PartialApply4Of5[Function4, R, S, T, U]#Apply] = pointed[PartialApply4Of5[Function4, R, S, T, U]#Apply](Function4Functor, Function4Pure)

  implicit def Function5Pointed[R, S, T, U, V]: Pointed[PartialApply5Of6[Function5, R, S, T, U, V]#Apply] = pointed[PartialApply5Of6[Function5, R, S, T, U, V]#Apply](Function5Functor, Function5Pure)

  implicit def Function6Pointed[R, S, T, U, V, W]: Pointed[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply] = pointed[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply](Function6Functor, Function6Pure)

  implicit def EitherLeftPointed[X]: Pointed[PartialApply1Of2[Either.LeftProjection, X]#Flip] = pointed[PartialApply1Of2[Either.LeftProjection, X]#Flip](EitherLeftFunctor, EitherLeftPure)
  
  implicit def EitherRightPointed[X]: Pointed[PartialApply1Of2[Either.RightProjection, X]#Apply] = pointed[PartialApply1Of2[Either.RightProjection, X]#Apply](EitherRightFunctor, EitherRightPure)
  
  implicit def ValidationPointed[X: Semigroup]: Pointed[PartialApply1Of2[Validation, X]#Apply] = pointed[PartialApply1Of2[Validation, X]#Apply](ValidationFunctor, ValidationPure)

  implicit def ValidationFailurePointed[X]: Pointed[PartialApply1Of2[FailProjection, X]#Flip] = pointed[PartialApply1Of2[FailProjection, X]#Flip](ValidationFailureFunctor, ValidationFailurePure)

  import java.util.Map.Entry

  implicit def MapEntryPointed[X: Monoid]: Pointed[PartialApply1Of2[Entry, X]#Apply] = pointed[PartialApply1Of2[Entry, X]#Apply](MapEntryFunctor, MapEntryPure)
}
