package scalaz

trait PartialWrapMMA[M[_], V[_[_], _]] {
  def apply[A](a: M[M[A]]): V[M, A]
}
