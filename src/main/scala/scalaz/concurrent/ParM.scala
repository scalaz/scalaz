package scalaz.concurrent

trait ParM[M[_]] {
  def apply[A](as: M[() => A])(implicit m: Functor[M], s: Strategy[A]): () => M[A] = {
    val v = m.fmap(as, s)
    () => m.fmap(v, (_: (() => A)).apply)
  }
}

object ParM {
  def parM[M[_]] = new ParM[M]{}
}