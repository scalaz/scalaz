package scalaz

sealed trait Function2W[-T1, -T2, R] {
  val k: (T1, T2) => R

  def flip = (v2: T2, v1: T1) => k(v1, v2)
}

object Function2W {
  implicit def Function2To[T1, T2, R](f: (T1, T2) => R): Function2W[T1, T2, R] = new Function2W[T1, T2, R] {
    val k = f
  }

  implicit def Function2From[T1, T2, R](f: Function2W[T1, T2, R]) = f.k
}
