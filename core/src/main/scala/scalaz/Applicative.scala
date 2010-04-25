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
  override def fmap[A, B](fa: Z[A], f: A => B) = this(pure(f), fa)
}

object Applicative {
  implicit def applicative[Z[_]](implicit p: Pure[Z], a: Apply[Z]): Applicative[Z] = new Applicative[Z] {
    def pure[A](a: => A) = p.pure(a)
    def apply[A, B](f: Z[A => B], x: Z[A]) = a(f, x)
  }

  import Pure._
  import Apply._
  import Scalaz._

  implicit def MonoidalApplicative[B: Monoid]: Applicative[PartialApply1Of2[Const, B]#Apply]
      = applicative[PartialApply1Of2[Const, B]#Apply](ConstPure, ConstApply)

  implicit def StateApplicative[A] = applicative[PartialApply1Of2[State, A]#Apply](StatePure, StateApply)

  implicit def Tuple2Applicative[R: Monoid]: Applicative[PartialApply1Of2[Tuple2, R]#Apply] = applicative[PartialApply1Of2[Tuple2, R]#Apply](Tuple2Pure, Tuple2Apply)

  implicit def Tuple3Applicative[R: Monoid, S: Monoid]: Applicative[PartialApply2Of3[Tuple3, R, S]#Apply] = applicative[PartialApply2Of3[Tuple3, R, S]#Apply](Tuple3Pure, Tuple3Apply)

  implicit def Tuple4Applicative[R: Monoid, S: Monoid, T: Monoid]: Applicative[PartialApply3Of4[Tuple4, R, S, T]#Apply] = applicative[PartialApply3Of4[Tuple4, R, S, T]#Apply](Tuple4Pure, Tuple4Apply)

  implicit def Tuple5Applicative[R: Monoid, S: Monoid, T: Monoid, U: Monoid]: Applicative[PartialApply4Of5[Tuple5, R, S, T, U]#Apply] = applicative[PartialApply4Of5[Tuple5, R, S, T, U]#Apply](Tuple5Pure, Tuple5Apply)

  implicit def Tuple6Applicative[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid]: Applicative[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply] = applicative[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply](Tuple6Pure, Tuple6Apply)

  implicit def Tuple7Applicative[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid, W: Monoid]: Applicative[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply] = applicative[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply](Tuple7Pure, Tuple7Apply)
  
  implicit def Function1Applicative[R]: Applicative[PartialApply1Of2[Function1, R]#Apply] = applicative[PartialApply1Of2[Function1, R]#Apply](Function1Pure, Function1Apply)

  implicit def Function2Applicative[R, S]: Applicative[PartialApply2Of3[Function2, R, S]#Apply] = applicative[PartialApply2Of3[Function2, R, S]#Apply](Function2Pure, Function2Apply)

  implicit def Function3Applicative[R, S, T]: Applicative[PartialApply3Of4[Function3, R, S, T]#Apply] = applicative[PartialApply3Of4[Function3, R, S, T]#Apply](Function3Pure, Function3Apply)

  implicit def Function4Applicative[R, S, T, U]: Applicative[PartialApply4Of5[Function4, R, S, T, U]#Apply] = applicative[PartialApply4Of5[Function4, R, S, T, U]#Apply](Function4Pure, Function4Apply)

  implicit def Function5Applicative[R, S, T, U, V]: Applicative[PartialApply5Of6[Function5, R, S, T, U, V]#Apply] = applicative[PartialApply5Of6[Function5, R, S, T, U, V]#Apply](Function5Pure, Function5Apply)

  implicit def Function6Applicative[R, S, T, U, V, W]: Applicative[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply] = applicative[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply](Function6Pure, Function6Apply)

  implicit def EitherLeftApplicative[X]: Applicative[PartialApply1Of2[Either.LeftProjection, X]#Flip] = applicative[PartialApply1Of2[Either.LeftProjection, X]#Flip](EitherLeftPure, EitherLeftApply)
  
  implicit def EitherRightApplicative[X]: Applicative[PartialApply1Of2[Either.RightProjection, X]#Apply] = applicative[PartialApply1Of2[Either.RightProjection, X]#Apply](EitherRightPure, EitherRightApply)
  
  implicit def ValidationApplicative[X: Semigroup]: Applicative[PartialApply1Of2[Validation, X]#Apply] = applicative[PartialApply1Of2[Validation, X]#Apply](ValidationPure, ValidationApply)

  implicit def ValidationFailureApplicative[X]: Applicative[PartialApply1Of2[FailProjection, X]#Flip] = applicative[PartialApply1Of2[FailProjection, X]#Flip](ValidationFailurePure, ValidationFailureApply)

  import java.util.Map.Entry

  implicit def MapEntryApplicative[X: Monoid]: Applicative[PartialApply1Of2[Entry, X]#Apply] = applicative[PartialApply1Of2[Entry, X]#Apply](MapEntryPure, MapEntryApply)
}
