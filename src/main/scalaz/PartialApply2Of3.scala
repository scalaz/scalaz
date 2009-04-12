package scalaz

trait PartialApply2Of3[T[_, _, _], A, B] {
  type Apply[C] = T[A, B, C]
}
