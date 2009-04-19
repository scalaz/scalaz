package scalaz

trait PartialApplyKA[T[_[_], _, _], M[_], A] {
  type Apply[B] = T[M, A, B]
}
