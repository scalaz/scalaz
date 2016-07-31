package scalaz
package typeclass

trait MonadClass[M[_]] extends Monad[M] with BindClass[M] with ApplicativeClass[M] {
  final def monad: Monad[M] = this
}

object MonadClass {
  trait Template[M[_]] extends MonadClass[M] with Monad.Map[M]
}
