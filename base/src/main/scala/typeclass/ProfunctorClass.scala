package scalaz
package typeclass

trait ProfunctorClass[F[_, _]] extends Profunctor[F] {
  final def profunctor: Profunctor[F] = this
}