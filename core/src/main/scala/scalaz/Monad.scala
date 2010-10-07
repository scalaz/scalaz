package scalaz

/**
 * Abstract a model that sequences computation through an environment.
 *
 * <p>
 * All monad instances must satisfy 3 laws:
 * <ol>
 * <li><strong>left identity</strong><br/><code>forall a f. f(a) == bind(pure(a), f)</code></li>
 * <li><strong>right identity</strong><br/><code>forall a. a == bind(a, x => pure(x))</code></li>
 * <li><strong>associativity</strong><br/><code>forall a f g. bind(a, x => bind(f(x), g)) == bind(bind(a, f), g)</code></li>
 * </p>
 */
trait Monad[M[_]] extends Applicative[M] with Bind[M] with Pointed[M] {
  override def fmap[A, B](fa: M[A], f: A => B) = bind(fa, (a: A) => pure(f(a)))
  override def apply[A, B](f: => M[A => B], a: => M[A]): M[B] = {
    lazy val fv = f
    lazy val av = a
    bind(fv, (k: A => B) => fmap(av, k(_: A)))
  }
}

object Monad {
  implicit def monad[M[_]](implicit b: Bind[M], p: Pure[M]): Monad[M] = new Monad[M] {
    override def pure[A](a: => A) = p.pure(a)
    override def bind[A, B](a: M[A], f: A => M[B]) = b.bind(a, f)
  }

  import Bind._
  import Pure._
  import Scalaz._

  implicit def StateMonad[A] = monad[PartialApply1Of2[State, A]#Apply](StateBind, StatePure)

  implicit def StateTMonad[M[_], A](implicit b: Bind[M], p: Pure[M]) = monad[PartialApplyKA[StateT, M, A]#Apply](StateTBind(b), StateTPure(p))

  implicit def Tuple2Monad[R: Monoid]: Monad[PartialApply1Of2[Tuple2, R]#Apply] = monad[PartialApply1Of2[Tuple2, R]#Apply](Tuple2Bind, Tuple2Pure)

  implicit def Tuple3Monad[R: Monoid, S: Monoid]: Monad[PartialApply2Of3[Tuple3, R, S]#Apply] = monad[PartialApply2Of3[Tuple3, R, S]#Apply](Tuple3Bind, Tuple3Pure)

  implicit def Tuple4Monad[R: Monoid, S: Monoid, T: Monoid]: Monad[PartialApply3Of4[Tuple4, R, S, T]#Apply] = monad[PartialApply3Of4[Tuple4, R, S, T]#Apply](Tuple4Bind, Tuple4Pure)

  implicit def Tuple5Monad[R: Monoid, S: Monoid, T: Monoid, U: Monoid]: Monad[PartialApply4Of5[Tuple5, R, S, T, U]#Apply] = monad[PartialApply4Of5[Tuple5, R, S, T, U]#Apply](Tuple5Bind, Tuple5Pure)

  implicit def Tuple6Monad[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid]: Monad[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply] = monad[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply](Tuple6Bind, Tuple6Pure)

  implicit def Tuple7Monad[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid, W: Monoid]: Monad[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply] = monad[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply](Tuple7Bind, Tuple7Pure)
  
  implicit def Function1Monad[R]: Monad[PartialApply1Of2[Function1, R]#Apply] = monad[PartialApply1Of2[Function1, R]#Apply](Function1Bind, Function1Pure)

  implicit def Function2Monad[R, S]: Monad[PartialApply2Of3[Function2, R, S]#Apply] = monad[PartialApply2Of3[Function2, R, S]#Apply](Function2Bind, Function2Pure)

  implicit def Function3Monad[R, S, T]: Monad[PartialApply3Of4[Function3, R, S, T]#Apply] = monad[PartialApply3Of4[Function3, R, S, T]#Apply](Function3Bind, Function3Pure)

  implicit def Function4Monad[R, S, T, U]: Monad[PartialApply4Of5[Function4, R, S, T, U]#Apply] = monad[PartialApply4Of5[Function4, R, S, T, U]#Apply](Function4Bind, Function4Pure)

  implicit def Function5Monad[R, S, T, U, V]: Monad[PartialApply5Of6[Function5, R, S, T, U, V]#Apply] = monad[PartialApply5Of6[Function5, R, S, T, U, V]#Apply](Function5Bind, Function5Pure)

  implicit def Function6Monad[R, S, T, U, V, W]: Monad[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply] = monad[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply](Function6Bind, Function6Pure)

  implicit def EitherLeftMonad[X]: Monad[PartialApply1Of2[Either.LeftProjection, X]#Flip] = monad[PartialApply1Of2[Either.LeftProjection, X]#Flip](EitherLeftBind, EitherLeftPure)
  
  implicit def EitherRightMonad[X]: Monad[PartialApply1Of2[Either.RightProjection, X]#Apply] = monad[PartialApply1Of2[Either.RightProjection, X]#Apply](EitherRightBind, EitherRightPure)
  
  implicit def ValidationMonad[X]: Monad[PartialApply1Of2[Validation, X]#Apply] = monad[PartialApply1Of2[Validation, X]#Apply](ValidationBind, ValidationPure)

  implicit def ValidationFailureMonad[X]: Monad[PartialApply1Of2[FailProjection, X]#Flip] = monad[PartialApply1Of2[FailProjection, X]#Flip](ValidationFailureBind, ValidationFailurePure)

  implicit def IterVMonad[E] = monad[PartialApply1Of2[IterV, E]#Apply](IterVBind, IterVPure)

  import java.util.Map.Entry

  implicit def MapEntryBind[X: Monoid]: Monad[PartialApply1Of2[Entry, X]#Apply] = monad[PartialApply1Of2[Entry, X]#Apply](MapEntryBind, MapEntryPure)  
}
