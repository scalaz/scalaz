package scalaz

trait PartialApply4Of5[T[_, _, _, _, _], A, B, C, D] {
  type Apply[E] = T[A, B, C, D, E]
}
