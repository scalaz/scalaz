package scalaz

sealed trait Function1W[T, R] {
  val k: T => R

  import Scalaz._

  def on[X](f: (R, R) => X, t1: T, t2: T): X = f(k(t1), k(t2))

  def arrow[A[_, _]](implicit a: Arrow[A]): A[T, R] = a arrow k

  def kleisli[Z[_]](implicit z: Pure[Z]): Kleisli[Z, T, R] = Scalaz.kleisli((t: T) => z pure k(t))

  def unary_!(implicit m: Memo[T, R]): (T) => R = m(k)

  import concurrent.Strategy
  import concurrent.Promise
  
  def promise(implicit s: Strategy): Kleisli[Promise, T, R] = kleisli[Promise]

  def lift[F[_]](implicit f: Functor[F]): (F[T]) => F[R] = (x: F[T]) => x.map(this)

  def toValidation[E](error: => E)(implicit ev: R <:< Boolean): T => Validation[NonEmptyList[E], T] = (t: T) => (k(t): Boolean).option(t).toSuccess(error.wrapNel); 

  def byName: (=> T) => R = t => k(t)

  def endo(implicit ev: R =:= T): Endo[T] = EndoTo(k ∘ ev)
}

trait Function1s {
  implicit def Function1To[T, R](f: T => R): Function1W[T, R] = new Function1W[T, R] {
    val k = f
  }

  implicit def Function1From[T, R](f: Function1W[T, R]): (T) => R = f.k
}
