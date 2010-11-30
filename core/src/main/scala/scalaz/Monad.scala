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

  implicit def StateMonad[A] = monad[({type λ[α]=State[A, α]})#λ](StateBind, StatePure)

  implicit def StateTMonad[M[_], A](implicit b: Bind[M], p: Pure[M]) = monad[({type λ[α]=StateT[M, A, α]})#λ](StateTBind(b), StateTPure(p))

  implicit def Tuple2Monad[R: Monoid]: Monad[({type λ[α]=(R, α)})#λ] = monad[({type λ[α]=(R, α)})#λ](Tuple2Bind, Tuple2Pure)

  implicit def Tuple3Monad[R: Monoid, S: Monoid]: Monad[({type λ[α]=(R, S, α)})#λ] = monad[({type λ[α]=(R, S, α)})#λ](Tuple3Bind, Tuple3Pure)

  implicit def Tuple4Monad[R: Monoid, S: Monoid, T: Monoid]: Monad[({type λ[α]=(R, S, T, α)})#λ] = monad[({type λ[α]=(R, S, T, α)})#λ](Tuple4Bind, Tuple4Pure)

  implicit def Tuple5Monad[R: Monoid, S: Monoid, T: Monoid, U: Monoid]: Monad[({type λ[α]=(R, S, T, U, α)})#λ] = monad[({type λ[α]=(R, S, T, U, α)})#λ](Tuple5Bind, Tuple5Pure)

  implicit def Tuple6Monad[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid]: Monad[({type λ[α]=(R, S, T, U, V, α)})#λ] = monad[({type λ[α]=(R, S, T, U, V, α)})#λ](Tuple6Bind, Tuple6Pure)

  implicit def Tuple7Monad[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid, W: Monoid]: Monad[({type λ[α]=(R, S, T, U, V, W, α)})#λ] = monad[({type λ[α]=(R, S, T, U, V, W, α)})#λ](Tuple7Bind, Tuple7Pure)
  
  implicit def Function1Monad[R]: Monad[({type λ[α]=(R) => α})#λ] = monad[({type λ[α]=(R) => α})#λ](Function1Bind, Function1Pure)

  implicit def Function2Monad[R, S]: Monad[({type λ[α]=(R, S) => α})#λ] = monad[({type λ[α]=(R, S) => α})#λ](Function2Bind, Function2Pure)

  implicit def Function3Monad[R, S, T]: Monad[({type λ[α]=(R, S, T) => α})#λ] = monad[({type λ[α]=(R, S, T) => α})#λ](Function3Bind, Function3Pure)

  implicit def Function4Monad[R, S, T, U]: Monad[({type λ[α]=(R, S, T, U) => α})#λ] = monad[({type λ[α]=(R, S, T, U) => α})#λ](Function4Bind, Function4Pure)

  implicit def Function5Monad[R, S, T, U, V]: Monad[({type λ[α]=(R, S, T, U, V) => α})#λ] = monad[({type λ[α]=(R, S, T, U, V) => α})#λ](Function5Bind, Function5Pure)

  implicit def Function6Monad[R, S, T, U, V, W]: Monad[({type λ[α]=(R, S, T, U, V, W) => α})#λ] = monad[({type λ[α]=(R, S, T, U, V, W) => α})#λ](Function6Bind, Function6Pure)

  implicit def EitherLeftMonad[X]: Monad[({type λ[α]=Either.LeftProjection[α, X]})#λ] = monad[({type λ[α]=Either.LeftProjection[α, X]})#λ](EitherLeftBind, EitherLeftPure)
  
  implicit def EitherRightMonad[X]: Monad[({type λ[α]=Either.RightProjection[X, α]})#λ] = monad[({type λ[α]=Either.RightProjection[X, α]})#λ](EitherRightBind, EitherRightPure)
  
  implicit def ValidationMonad[X]: Monad[({type λ[α]=Validation[X, α]})#λ] = monad[({type λ[α]=Validation[X, α]})#λ](ValidationBind, ValidationPure)

  implicit def ValidationFailureMonad[X]: Monad[({type λ[α]=FailProjection[α, X]})#λ] = monad[({type λ[α]=FailProjection[α, X]})#λ](ValidationFailureBind, ValidationFailurePure)

  implicit def IterVMonad[E] = monad[({type λ[α]=IterV[E, α]})#λ](IterVBind, IterVPure)

  import java.util.Map.Entry

  implicit def MapEntryBind[X: Monoid]: Monad[({type λ[α]=Entry[X, α]})#λ] = monad[({type λ[α]=Entry[X, α]})#λ](MapEntryBind, MapEntryPure)  
}
