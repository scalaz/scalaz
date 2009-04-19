package scalaz

trait PartialApplyK[T[_[_], _, _], M[_]] {
  type Apply[A, B] = T[M, A, B]
}
