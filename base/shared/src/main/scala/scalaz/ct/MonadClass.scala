package scalaz
package ct

trait MonadClass[M[_]] extends ApplicativeClass[M] with BindClass[M]

object MonadClass {

  trait DeriveMap[M[_]] extends MonadClass[M] with BindClass.Alt[BindClass.DeriveFlatten[M]] {
    final override def map[A, B](ma: M[A])(f: (A) => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  trait Alt[D <: Alt[D]]
}
