package scalaz
package data

import scalaz.typeclass.{ BindClass, DebugClass, MonadClass }

trait DisjunctionInstances {
  implicit def disjunctionMonad[L]: Monad[L \/ ?] =
    instanceOf(new MonadClass[L \/ ?] with BindClass.DeriveFlatten[L \/ ?] {

      override def map[A, B](ma: L \/ A)(f: A => B): L \/ B =
        ma.fold[L \/ B](l => -\/(l))(r => \/-(f(r)))

      override def ap[A, B](ma: L \/ A)(mf: L \/ (A => B)): L \/ B =
        ma.fold[L \/ B](l => -\/(l))(a => map[(A => B), B](mf)(f => f(a)))

      override def pure[A](a: A): L \/ A =
        \/-[L, A](a)

      override def flatMap[A, B](oa: L \/ A)(f: A => L \/ B): L \/ B =
        oa.fold[L \/ B](l => -\/(l))(a => f(a))
    })

  implicit def disjunctionDebug[L, R](implicit L: Debug[L], R: Debug[R]): Debug[L \/ R] =
    instanceOf[DebugClass[L \/ R]] {
      case -\/(left)  => s"""-\/(${L.debug(left)})"""
      case \/-(right) => s"""\/-(${R.debug(right)})"""
    }
}
