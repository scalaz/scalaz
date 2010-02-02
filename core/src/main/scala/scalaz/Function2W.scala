package scalaz

sealed trait Function2W[T1, T2, R] {
  val k: (T1, T2) => R

  def flip = (v2: T2, v1: T1) => k(v1, v2)

  def on[X](f: (R, R) => X, t1: (T1, T1), t2: (T2, T2)) = f(k(t1._1, t2._1), k(t1._2, t2._2))

  import concurrent.Strategy
  import concurrent.Promise
  import Scalaz._

  def promise(implicit s: Strategy[Unit]) = (x: T1, y: T2) => x.pure[Promise] <**> (y.pure[Promise], k)
}

trait Function2s {
  implicit def Function2To[T1, T2, R](f: (T1, T2) => R): Function2W[T1, T2, R] = new Function2W[T1, T2, R] {
    val k = f
  }

  implicit def Function2From[T1, T2, R](f: Function2W[T1, T2, R]): Function2[T1, T2, R] = f.k
}
