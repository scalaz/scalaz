package scalaz
package clazz

trait FunctorClass[F[_]] extends Functor[F]{
  implicit final def functor: Functor[F] = this
}
