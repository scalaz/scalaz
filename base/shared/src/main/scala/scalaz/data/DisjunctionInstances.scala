package scalaz
package data

import typeclass.MonadClass

trait DisjunctionInstances {
  implicit def disjunctionMonad[L]: Monad[L \/ ?] = new MonadClass.Template[L \/ ?] {

    override def map[A, B](ma: L \/ A)(f: A => B): L \/ B =
      ma.fold[L \/ B](l => -\/(l))(r => \/-(f(r)))

    override def ap[A, B](ma: L \/ A)(mf: L \/ (A => B)): L \/ B =
      ma.fold[L \/ B](l => -\/(l))(a => map[(A => B), B](mf)(f => f(a)))

    override def pure[A](a: A): L \/ A =
      \/-[L, A](a)

    override def flatMap[A, B](oa: L \/ A)(f: A => L \/ B): L \/ B =
      oa.fold[L \/ B](l => -\/(l))(a => f(a))
  }

  implicit def disjunctionDebug[L, R](implicit L: Debug[L], R: Debug[R]): Debug[L \/ R] =
    Debug.fromDebugs {
      case -\/(left)  => s"""-\/(${L.debug(left)})"""
      case \/-(right) => s"""\/-(${R.debug(right)})"""
    }
}
