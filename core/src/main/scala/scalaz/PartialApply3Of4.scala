package scalaz

trait PartialApply3Of4[T[_, _, _, _], A, B, C] {
  type Apply[D] = T[A, B, C, D]
}
