package scalaz

trait Pointed[P[_]] extends Functor[P] with Pure[P]

trait PointedLow {
  implicit def pointed[P[_]](implicit t: Functor[P], p: Pure[P]): Pointed[P] = new Pointed[P] {
    def fmap[A, B](a: P[A], f: A => B) = t.fmap(a, f)

    def pure[A](a: => A): P[A] = p.pure(a)
  }
}

object Pointed extends PointedLow {
  import Functor._
  import Pure._

  implicit def StatePointed[A] =
    pointed[({type λ[α] = State[A, α]})#λ](StateFunctor, StatePure)

  implicit def Tuple2Pointed[R: Monoid] =
    pointed[({type λ[α] = (R, α)})#λ](Tuple2Functor, Tuple2Pure)

  implicit def Tuple3Pointed[R: Monoid, S: Monoid] =
    pointed[({type λ[α] = (R, S, α)})#λ](Tuple3Functor, Tuple3Pure)

  implicit def Tuple4Pointed[R: Monoid, S: Monoid, T: Monoid] =
    pointed[({type λ[α] = (R, S, T, α)})#λ](Tuple4Functor, Tuple4Pure)

  implicit def Tuple5Pointed[R: Monoid, S: Monoid, T: Monoid, U: Monoid] =
    pointed[({type λ[α] = (R, S, T, U, α)})#λ](Tuple5Functor, Tuple5Pure)

  implicit def Tuple6Pointed[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid] =
    pointed[({type λ[α] = (R, S, T, U, V, α)})#λ](Tuple6Functor, Tuple6Pure)

  implicit def Tuple7Pointed[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid, W: Monoid] =
    pointed[({type λ[α] = (R, S, T, U, V, W, α)})#λ](Tuple7Functor, Tuple7Pure)

  implicit def Function1Pointed[R] =
    pointed[({type λ[α] = (R) => α})#λ](Function1Functor, Function1Pure)

  implicit def Function2Pointed[R, S] =
    pointed[({type λ[α] = (R, S) => α})#λ](Function2Functor, Function2Pure)

  implicit def Function3Pointed[R, S, T] =
    pointed[({type λ[α] = (R, S, T) => α})#λ](Function3Functor, Function3Pure)

  implicit def Function4Pointed[R, S, T, U] =
    pointed[({type λ[α] = (R, S, T, U) => α})#λ](Function4Functor, Function4Pure)

  implicit def Function5Pointed[R, S, T, U, V] =
    pointed[({type λ[α] = (R, S, T, U, V) => α})#λ](Function5Functor, Function5Pure)

  implicit def Function6Pointed[R, S, T, U, V, W] =
    pointed[({type λ[α] = (R, S, T, U, V, W) => α})#λ](Function6Functor, Function6Pure)

  implicit def EitherLeftPointed[X] =
    pointed[({type λ[α] = Either.LeftProjection[α, X]})#λ](EitherLeftFunctor, EitherLeftPure)

  implicit def EitherRightPointed[X] =
    pointed[({type λ[α] = Either.RightProjection[X, α]})#λ](EitherRightFunctor, EitherRightPure)

  implicit def ValidationPointed[X: Semigroup] =
    pointed[({type λ[α] = Validation[X, α]})#λ](ValidationFunctor, ValidationPure)

  implicit def ValidationFailurePointed[X] =
    pointed[({type λ[α] = FailProjection[α, X]})#λ](ValidationFailureFunctor, ValidationFailurePure)

  import java.util.Map.Entry

  implicit def MapEntryPointed[X: Monoid] =
    pointed[({type λ[α] = Entry[X, α]})#λ](MapEntryFunctor, MapEntryPure)
}
