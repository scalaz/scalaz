package scalaz
package wrap


sealed trait Function1W[T, R] {
  val k: T => R

  def *->* : (({type λ[α] = Function1[T, α]})#λ *->* R) =
    scalaz.*->*.!**->**![({type λ[α] = Function1[T, α]})#λ, R](k)

  import concurrent.{Promise, Strategy}
  import Validation._, NonEmptyList._

  def on[X](f: (R, R) => X, t1: T, t2: T): X = f(k(t1), k(t2))

  def arrow[A[_, _]](implicit a: Arr[A]): A[T, R] =
    a.arr(k)

  def kleisli[Z[_]](implicit z: Pointed[Z]): Kleisli[T, Z, R] =
    Kleisli.kleisli((t: T) => z.point(k(t)))

  def unary_!(implicit m: Memo[T, R]): (T) => R = m(k)

  def promise(implicit s: Strategy): Kleisli[T, Promise, R] = kleisli[Promise]

  def fmap[F[_]](implicit f: Functor[F]): (F[T]) => F[R] =
    (x: F[T]) => f.fmap((t: T) => k(t))(x)

  def toValidation[E](e: => E)(implicit ev: R =:= Boolean): T => Validation[NonEmptyList[E], T] =
    (t: T) => (if (k(t): Boolean) success(t) else failure(nel(e, Nil)))

  def byName: (=> T) => R = t => k(t)

  def endo(implicit ev: R =:= T): Endo[T] =
    Endo.endo(t => ev(k(t)))

  def comparing(implicit o: Order[R]): (T, T) => Ordering =
    (t1, t2) => o.order(k(t1))(k(t2))

  def equaling(implicit e: Equal[R]): (T, T) => Boolean =
    (t1, t2) => e.equal(k(t1))(k(t2))

  import effect._

  def withResource[K](
                         value: => T
                         , whenComputing: Throwable => IO[R] = (t: Throwable) => throw t
                         , whenClosing: Throwable => IO[Unit] = _ => IO.ioUnit
                         )(implicit r: Resource[T]): IO[R] =
    try {
      val u = value
      try {
        val r = k(u)
        IO(r)
      } finally {
        try {
          r close u
        } catch {
          case ex => whenClosing(ex)
        }
      }
    } catch {
      case ex => whenComputing(ex)
    }
}

object Function1W extends Function1Ws

trait Function1Ws {
  implicit def Function1To[T, R](f: T => R): Function1W[T, R] = new Function1W[T, R] {
    val k = f
  }

  implicit def Function1From[T, R](f: Function1W[T, R]): (T) => R = f.k
}
