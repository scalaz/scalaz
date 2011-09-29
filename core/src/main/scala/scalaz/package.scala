package object scalaz {
  type Id[X] = X

  // Unboxed newtypes, credit to Miles Sabin.
  type Tagged[T] = {type Tag = T}
//  type NewType[T, Tag] = T with Tagged[T]
  type ##[T, Tag] = T with Tagged[T]

  type ~>[F[_], G[_]] = NaturalTransformation[F, G]
}