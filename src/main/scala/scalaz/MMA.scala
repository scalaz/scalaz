package scalaz

sealed trait MMA[M[_], A] {
  val v: M[M[A]]

  def join(implicit b: Bind[M]) = b.bind(v, (x: M[A]) => x)
}

object MMA {
  implicit def mma[M[_]] = new PartialWrapMMA[M, MMA] {
    def apply[A](m: M[M[A]]) = new MMA[M, A] {
      val v = m
    }
  }
}
