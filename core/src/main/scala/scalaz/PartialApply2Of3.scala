package scalaz

trait PartialApply2Of3[T[_, _, _], A, B] {
  type Apply[C] = T[A, B, C]

  type ApplyB[C] = T[A, C, B]

  type ApplyA[C] = T[C, A, B] 
}
