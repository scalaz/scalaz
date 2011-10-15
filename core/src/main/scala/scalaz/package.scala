package object scalaz {
  type Id[X] = X

  // Unboxed newtypes, credit to Miles Sabin.
  type Tagged[T] = {type Tag = T}
  type @@[T, Tag] = T with Tagged[Tag]

  type ~>[F[_], G[_]] = NaturalTransformation[F, G]
  type <~[F[_], G[_]] = NaturalTransformation[G, F]
  type ~~>[F[_,_], G[_,_]] = BinaturalTransformation[F, G]

  
}