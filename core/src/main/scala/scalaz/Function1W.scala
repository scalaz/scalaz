package scalaz

sealed trait Function1W[T, R] {
  val k: T => R

  import Scalaz._

  def on[X](f: (R, R) => X, t1: T, t2: T) = f(k(t1), k(t2))

  def arrow[A[_, _]](implicit a: Arrow[A]) = a arrow k

  def kleisli[Z[_]: Pure]: Kleisli[Z, T, R] = ☆(k(_) η)

  def unary_!(implicit m: Memo[T, R]) = m(k)

  import concurrent.Strategy
  
  def concurry(implicit s: Strategy[R]): T => () => R = (t: T) => s(() => k(t))

  def toValidation[E](error: => E)(implicit ev: R <:< Boolean): T => Validation[NonEmptyList[E], T] = (t: T) => ev(k(t)).option(t).toSuccess(error.nel); 
}

trait Function1s {
  implicit def Function1To[T, R](f: T => R): Function1W[T, R] = new Function1W[T, R] {
    val k = f
  }

  implicit def Function1From[T, R](f: Function1W[T, R]): Function1[T, R] = f.k
}
