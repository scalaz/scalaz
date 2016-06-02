package scalaz
package typeclass

trait FunctorClass[F[_]] extends Functor[F]{
  implicit final def functor: Functor[F] = this
}
