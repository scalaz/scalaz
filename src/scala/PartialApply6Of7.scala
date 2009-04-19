package scalaz

trait PartialApply6Of7[T[_, _, _, _, _, _, _], A, B, C, D, E, F] {
  type Apply[G] = T[A, B, C, D, E, F, G]
}
