package scalaz
package ct

trait BindClass[M[_]] extends ApplyClass[M] {
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def flatten[A](ma: M[M[A]]): M[A]
}

object BindClass {

  trait DeriveAp[M[_]] extends BindClass[M] with MonadClass.Alt[DeriveAp[M]] {
    final override def ap[A, B](fa: M[A])(f: M[A => B]): M[B] = flatMap(f)(map(fa))
  }

  trait DeriveFlatten[M[_]] extends BindClass[M] with Alt[DeriveFlatten[M]] {
    final override def flatten[A](ma: M[M[A]]): M[A] = flatMap(ma)(identity)
  }

  trait DeriveFlatMap[M[_]] extends BindClass[M] with Alt[DeriveFlatMap[M]] {
    final override def flatMap[A, B](ma: M[A])(f: (A) => M[B]): M[B] = flatten(map(ma)(f))
  }

  trait Alt[D <: Alt[D]]

}
