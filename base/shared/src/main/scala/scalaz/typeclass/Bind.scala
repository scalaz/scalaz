package scalaz
package typeclass

sealed trait Bind[M[_]] extends Bind.Class[M]

object Bind {

  trait Class[M[_]] extends Apply.Class[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def flatten[A](ma: M[M[A]]): M[A]

    def bind: Bind[M]
  }

  trait Template[M[_]] extends Apply.Template[M] with Bind[M] {
    final override def bind = this
  }

  trait DeriveAp[M[_]] extends Monad.Alt[DeriveAp[M]] { self: Class[M] =>
    final override def ap[A, B](fa: M[A])(f: M[A => B]): M[B] = flatMap(f)(map(fa))
  }

  trait Alt[D <: Alt[D]]
  trait DeriveFlatten[M[_]] extends Alt[DeriveFlatten[M]] { self: Class[M] =>
    final override def flatten[A](ma: M[M[A]]): M[A] = flatMap(ma)(identity)
  }
  trait DeriveFlatMap[M[_]] extends Alt[DeriveFlatMap[M]] { self: Class[M] =>
    final override def flatMap[A, B](ma: M[A])(f: (A) => M[B]): M[B] = flatten(map(ma)(f))
  }

  def apply[F[_]](implicit F: Bind[F]): Bind[F] = F
}