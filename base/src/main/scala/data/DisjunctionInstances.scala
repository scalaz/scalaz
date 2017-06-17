package scalaz
package data

import typeclass._
import Disjunction.{\/, -\/, \/-}

trait DisjunctionInstances {
  implicit def monad[L]: Monad[L \/ ?] = new MonadClass.Template[L \/ ?] {

    override def map[A, B](ma: L \/ A)(f: A => B): L \/ B =
      ma.fold[L \/ B](l => -\/(l))(r => \/-(f(r)))

    override def mapConst[A, B](ma: L \/ A)(c: B): L \/ B =
      ma.fold[L \/ B](-\/(_))(_ => \/-(c))

    override def ap[A, B](ma: L \/ A)(mf: L \/ (A => B)): L \/ B =
      ma.fold[L \/ B](l => -\/(l))(a => map[(A => B), B](mf)(f => f(a)))

    override def pure[A](a: A): L \/ A =
      \/-[L, A](a)

    override def flatMap[A, B](oa: L \/ A)(f: A => L \/ B): L \/ B =
      oa.fold[L \/ B](l => -\/(l))(a => f(a))

    override def tapM[A, B](ma: L \/ A)(f: (A) => L \/ B): L \/ A =
      ma.fold[L \/ A](-\/(_))(f(_).fold[L \/ A](-\/(_))(_ => ma))
  }
}
