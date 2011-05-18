package scalaz
package wrap

sealed trait Function2W[T1, T2, R] {
  val k: (T1, T2) => R

  def flip: (T2, T1) => R = (v2: T2, v1: T1) => k(v1, v2)

  def on[X](f: (R, R) => X, t1: (T1, T1), t2: (T2, T2)): X = f(k(t1._1, t2._1), k(t1._2, t2._2))

  import concurrent.Strategy
  import concurrent.Promise

  def promise(implicit s: Strategy): (T1, T2) => Promise[R] =
    (t1: T1, t2: T2) => Promise.promise(k(t1, t2))

  def contramap[TT](f: TT => T1)(implicit ev: T1 =:= T2): (TT, TT) => R =
    (t1, t2) => k(f(t1), ev(f(t2)))

  def lift2[F[_]](implicit f: Applicative[F]): (F[T1], F[T2]) => F[R] =
    (t1: F[T1], t2: F[T2]) => f.liftA2((a: T1) => (b: T2) => k(a, b))(t1)(t2)


  def byName: (=> T1, => T2) => R =
    (t1, t2) => k(t1, t2)
}

object Function2W extends Function2Ws

trait Function2Ws {
  implicit def Function2To[T1, T2, R](f: (T1, T2) => R): Function2W[T1, T2, R] = new Function2W[T1, T2, R] {
    val k = f
  }

  implicit def Function2From[T1, T2, R](f: Function2W[T1, T2, R]): (T1, T2) => R = f.k
}
