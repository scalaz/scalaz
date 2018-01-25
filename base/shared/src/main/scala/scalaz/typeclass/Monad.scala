package scalaz
package typeclass

sealed trait Monad[M[_]] extends Monad.Class[M]

object Monad {

  trait Class[M[_]] extends Applicative.Class[M] with Bind.Class[M] {
    def monad: Monad[M]
  }

  trait Template[M[_]] extends Applicative.Template[M] with Bind.Template[M] with Monad[M] {
    final override def monad = this
  }

  trait DeriveMap[M[_]] extends Bind.Alt[Bind.DeriveFlatten[M]] { self: Class[M] =>
    final override def map[A, B](ma: M[A])(f: (A) => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  trait Alt[D <: Alt[D]]

  def apply[M[_]](implicit M: Monad[M]): Monad[M] = M

}