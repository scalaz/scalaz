package scalaz
package typeclass

trait InvariantFunctorClass[F[_]] extends InvariantFunctor[F]{
  final def functor: InvariantFunctor[F] = this
}
