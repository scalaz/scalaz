package scalaz
package typeclass

trait FunctorClass[F[_]] extends Functor[F]{
  final def functor: Functor[F] = this
}
