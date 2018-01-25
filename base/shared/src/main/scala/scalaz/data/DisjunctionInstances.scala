package scalaz
package data

trait DisjunctionInstances {
  implicit def disjunctionMonad[L]: Monad[L \/ ?] = new Monad.Template[L \/ ?] with Bind.DeriveFlatten[L \/ ?] {

    override def map[A, B](ma: L \/ A)(f: A => B): L \/ B =
      ma.fold[L \/ B](l => -\/(l))(r => \/-(f(r)))

    override def ap[A, B](ma: L \/ A)(mf: L \/ (A => B)): L \/ B =
      ma.fold[L \/ B](l => -\/(l))(a => map[(A => B), B](mf)(f => f(a)))

    override def pure[A](a: A): L \/ A =
      \/-[L, A](a)

    override def flatMap[A, B](oa: L \/ A)(f: A => L \/ B): L \/ B =
      oa.fold[L \/ B](l => -\/(l))(a => f(a))
  }

  implicit def disjunctionShow[L, R](implicit L: Show[L], R: Show[R]): Show[L \/ R] = {
    case -\/(left)  => s"""-\/(${L.show(left)})"""
    case \/-(right) => s"""\/-(${R.show(right)})"""
  }
}
