package scalaz
package data

trait OpticTypes {
  type Optic[P[_, _], F[_], S, T, A, B] = P[A, F[B]] => P[S, F[T]]
  type Optic_[P[_, _], F[_], S, A] = Optic[P, F, S, S, A, A]

  type Optical[P[_, _], Q[_, _], F[_], S, T, A, B] = P[A, F[B]] => Q[S, F[T]]
  type Optical_[P[_, _], Q[_, _], F[_], S, A] = Optical[P, Q, F, S, S, A, A]

  type Lens_[S, A] = Lens[S, S, A, A]
  type Prism_[S, A] = Prism[S, S, A, A]
  type Traversal_[S, A] = Traversal[S, S, A, A]
}
