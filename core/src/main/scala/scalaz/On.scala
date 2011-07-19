package scalaz

trait On[P[_, _], F[_]] {
  type Apply[A, B] = P[F[A], F[B]]
}
