package object scalaz {
  type Id[X] = X

  type ~>[F[_],G[_]] = NaturalTransformation[F, G]
}