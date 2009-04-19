package scalaz

trait PartialWrapMA[M[_], V[_[_], _]] {
  def apply[A](a: M[A]): V[M, A]
}
