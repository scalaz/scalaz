package scalaz

trait Biff[P[_, _], F[_], G[_]] {
  type Apply[A, B] = P[F[A], G[B]]
}
