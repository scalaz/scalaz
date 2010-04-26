package scalaz

trait PartialApply1Of3[T[_, _, _], A] {
  type Apply[B,C] = T[A, B, C]
}

