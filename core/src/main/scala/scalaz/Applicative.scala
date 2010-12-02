package scalaz

/**
 * Defines an applicative functor as described by McBride and Paterson in
 * <a href="http://www.soi.city.ac.uk/~ross/papers/Applicative.html">Applicative Programming with Effects</a>.
 *
 * <p>
 * All instances must satisfy 4 laws:
 * <ol>
 * <li><strong>identity</strong><br/><code>forall a. a == apply(a, pure(identity))</code></li>
 * <li><strong>composition</strong><br/><code>forall af ag a. apply(apply(a, ag), af) == apply(a, apply(ag, apply(af, pure(compose))))</code></li>
 * <li><strong>homomorphism</strong><br/><code>forall f a. apply(pure(a), pure(f)) == pure(f(a))</code></li>
 * <li><strong>interchange</strong><br/><code>forall af a. apply(pure(a), af) == apply(af, pure(f => f(x)))</code></li>
 * </ol>
 * </p>
 */
trait Applicative[Z[_]] extends Pointed[Z] with Apply[Z] {
  override def fmap[A, B](fa: Z[A], f: A => B): Z[B] = this(pure(f), fa)
}

object Applicative {
  implicit def applicative[Z[_]](implicit p: Pure[Z], a: Apply[Z]): Applicative[Z] = new Applicative[Z] {
    def pure[A](a: => A) = p.pure(a)

    def apply[A, B](f: => Z[A => B], x: => Z[A]) = a(f, x)
  }

  import Pure._
  import Apply._
  import Scalaz._

  implicit def StateApplicative[A] =
    applicative[( {type λ[α] = State[A, α]})#λ](StatePure, StateApply)

  implicit def Tuple2Applicative[R: Monoid]: Applicative[( {type λ[α] = (R, α)})#λ] =
    applicative[( {type λ[α] = (R, α)})#λ](Tuple2Pure, Tuple2Apply)

  implicit def Tuple3Applicative[R: Monoid, S: Monoid]: Applicative[( {type λ[α] = (R, S, α)})#λ] =
    applicative[( {type λ[α] = (R, S, α)})#λ](Tuple3Pure, Tuple3Apply)

  implicit def Tuple4Applicative[R: Monoid, S: Monoid, T: Monoid]: Applicative[( {type λ[α] = (R, S, T, α)})#λ] =
    applicative[( {type λ[α] = (R, S, T, α)})#λ](Tuple4Pure, Tuple4Apply)

  implicit def Tuple5Applicative[R: Monoid, S: Monoid, T: Monoid, U: Monoid]: Applicative[( {type λ[α] = (R, S, T, U, α)})#λ] =
    applicative[( {type λ[α] = (R, S, T, U, α)})#λ](Tuple5Pure, Tuple5Apply)

  implicit def Tuple6Applicative[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid]: Applicative[( {type λ[α] = (R, S, T, U, V, α)})#λ] =
    applicative[( {type λ[α] = (R, S, T, U, V, α)})#λ](Tuple6Pure, Tuple6Apply)

  implicit def Tuple7Applicative[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid, W: Monoid]: Applicative[( {type λ[α] = (R, S, T, U, V, W, α)})#λ] =
    applicative[( {type λ[α] = (R, S, T, U, V, W, α)})#λ](Tuple7Pure, Tuple7Apply)

  implicit def Function1Applicative[R]: Applicative[( {type λ[α] = (R) => α})#λ] =
    applicative[( {type λ[α] = (R) => α})#λ](Function1Pure, Function1Apply)

  implicit def Function2Applicative[R, S]: Applicative[( {type λ[α] = (R, S) => α})#λ] =
    applicative[( {type λ[α] = (R, S) => α})#λ](Function2Pure, Function2Apply)

  implicit def Function3Applicative[R, S, T]: Applicative[( {type λ[α] = (R, S, T) => α})#λ] =
    applicative[( {type λ[α] = (R, S, T) => α})#λ](Function3Pure, Function3Apply)

  implicit def Function4Applicative[R, S, T, U]: Applicative[( {type λ[α] = (R, S, T, U) => α})#λ] =
    applicative[( {type λ[α] = (R, S, T, U) => α})#λ](Function4Pure, Function4Apply)

  implicit def Function5Applicative[R, S, T, U, V]: Applicative[( {type λ[α] = (R, S, T, U, V) => α})#λ] =
    applicative[( {type λ[α] = (R, S, T, U, V) => α})#λ](Function5Pure, Function5Apply)

  implicit def Function6Applicative[R, S, T, U, V, W]: Applicative[( {type λ[α] = (R, S, T, U, V, W) => α})#λ] =
    applicative[( {type λ[α] = (R, S, T, U, V, W) => α})#λ](Function6Pure, Function6Apply)

  implicit def EitherLeftApplicative[X]: Applicative[PartialApply1Of2[Either.LeftProjection, X]#Flip] =
    applicative[PartialApply1Of2[Either.LeftProjection, X]#Flip](EitherLeftPure, EitherLeftApply)

  implicit def EitherRightApplicative[X]: Applicative[PartialApply1Of2[Either.RightProjection, X]#Apply] =
    applicative[PartialApply1Of2[Either.RightProjection, X]#Apply](EitherRightPure, EitherRightApply)

  implicit def ValidationApplicative[X: Semigroup]: Applicative[( {type λ[α] = Validation[X, α]})#λ] =
    applicative[( {type λ[α] = Validation[X, α]})#λ](ValidationPure, ValidationApply)

  implicit def ValidationFailureApplicative[X]: Applicative[( {type λ[α] = FailProjection[α, X]})#λ] =
    applicative[( {type λ[α] = FailProjection[α, X]})#λ](ValidationFailurePure, ValidationFailureApply)

  import java.util.Map.Entry

  implicit def MapEntryApplicative[X: Monoid]: Applicative[( {type λ[α] = Entry[X, α]})#λ] =
    applicative[( {type λ[α] = Entry[X, α]})#λ](MapEntryPure, MapEntryApply)

  implicit def MonoidalApplicative[B: Monoid]: Applicative[( {type λ[α] = Const[B, α]})#λ] =
    applicative[( {type λ[α] = Const[B, α]})#λ](ConstPure, ConstApply)
}
