package scalaz

trait PartialWrapMAB[M[_, _], V[_[_, _], _, _]] {
  def apply[A, B](a: M[A, B]): V[M, A, B]
}
