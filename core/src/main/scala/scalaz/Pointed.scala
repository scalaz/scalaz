package scalaz

trait Pointed[P[_]] extends Functor[P] with Pure[P]

object Pointed {
  implicit def pointed[P[_]](implicit t: Functor[P], p: Pure[P]) = new Pointed[P] {
    def fmap[A, B](a: P[A], f: A => B) = t.fmap(a, f)
    def pure[A](a: => A): P[A] = p.pure(a)
  }
  
  import Functor._
  import Pure._
  
  implicit def StatePointed[A] = pointed[({type λ[α]=State[A, α]})#λ](StateFunctor, StatePure)

  implicit def Tuple2Pointed[R: Monoid]: Pointed[({type λ[α]=(R, α)})#λ] = pointed[({type λ[α]=(R, α)})#λ](Tuple2Functor, Tuple2Pure)

  implicit def Tuple3Pointed[R: Monoid, S: Monoid]: Pointed[({type λ[α]=(R, S, α)})#λ] = pointed[({type λ[α]=(R, S, α)})#λ](Tuple3Functor, Tuple3Pure)

  implicit def Tuple4Pointed[R: Monoid, S: Monoid, T: Monoid]: Pointed[({type λ[α]=(R, S, T, α)})#λ] = pointed[({type λ[α]=(R, S, T, α)})#λ](Tuple4Functor, Tuple4Pure)

  implicit def Tuple5Pointed[R: Monoid, S: Monoid, T: Monoid, U: Monoid]: Pointed[({type λ[α]=(R, S, T, U, α)})#λ] = pointed[({type λ[α]=(R, S, T, U, α)})#λ](Tuple5Functor, Tuple5Pure)

  implicit def Tuple6Pointed[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid]: Pointed[({type λ[α]=(R, S, T, U, V, α)})#λ] = pointed[({type λ[α]=(R, S, T, U, V, α)})#λ](Tuple6Functor, Tuple6Pure)

  implicit def Tuple7Pointed[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid, W: Monoid]: Pointed[({type λ[α]=(R, S, T, U, V, W, α)})#λ] = pointed[({type λ[α]=(R, S, T, U, V, W, α)})#λ](Tuple7Functor, Tuple7Pure)
  
  implicit def Function1Pointed[R]: Pointed[({type λ[α]=(R) => α})#λ] = pointed[({type λ[α]=(R) => α})#λ](Function1Functor, Function1Pure)

  implicit def Function2Pointed[R, S]: Pointed[({type λ[α]=(R, S) => α})#λ] = pointed[({type λ[α]=(R, S) => α})#λ](Function2Functor, Function2Pure)

  implicit def Function3Pointed[R, S, T]: Pointed[({type λ[α]=(R, S, T) => α})#λ] = pointed[({type λ[α]=(R, S, T) => α})#λ](Function3Functor, Function3Pure)

  implicit def Function4Pointed[R, S, T, U]: Pointed[({type λ[α]=(R, S, T, U) => α})#λ] = pointed[({type λ[α]=(R, S, T, U) => α})#λ](Function4Functor, Function4Pure)

  implicit def Function5Pointed[R, S, T, U, V]: Pointed[({type λ[α]=(R, S, T, U, V) => α})#λ] = pointed[({type λ[α]=(R, S, T, U, V) => α})#λ](Function5Functor, Function5Pure)

  implicit def Function6Pointed[R, S, T, U, V, W]: Pointed[({type λ[α]=(R, S, T, U, V, W) => α})#λ] = pointed[({type λ[α]=(R, S, T, U, V, W) => α})#λ](Function6Functor, Function6Pure)

  implicit def EitherLeftPointed[X]: Pointed[PartialApply1Of2[Either.LeftProjection, X]#Flip] = pointed[PartialApply1Of2[Either.LeftProjection, X]#Flip](EitherLeftFunctor, EitherLeftPure)
  
  implicit def EitherRightPointed[X]: Pointed[PartialApply1Of2[Either.RightProjection, X]#Apply] = pointed[PartialApply1Of2[Either.RightProjection, X]#Apply](EitherRightFunctor, EitherRightPure)
  
  implicit def ValidationPointed[X: Semigroup]: Pointed[({type λ[α]=Validation[X, α]})#λ] = pointed[({type λ[α]=Validation[X, α]})#λ](ValidationFunctor, ValidationPure)

  implicit def ValidationFailurePointed[X]: Pointed[({type λ[α]=FailProjection[α, X]})#λ] = pointed[({type λ[α]=FailProjection[α, X]})#λ](ValidationFailureFunctor, ValidationFailurePure)

  import java.util.Map.Entry

  implicit def MapEntryPointed[X: Monoid]: Pointed[({type λ[α]=Entry[X, α]})#λ] = pointed[({type λ[α]=Entry[X, α]})#λ](MapEntryFunctor, MapEntryPure)
}
