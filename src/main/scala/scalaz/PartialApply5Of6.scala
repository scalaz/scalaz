package scalaz

trait PartialApply5Of6[T[_, _, _, _, _, _], A, B, C, D, E] {
  type Apply[F] = T[A, B, C, D, E, F]
}
