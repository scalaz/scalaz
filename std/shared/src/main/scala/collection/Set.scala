package scalaz
package std

import typeclass.{ EqClass, MonadClass }

trait SetInstances {
  implicit val setMonad: Monad[Set] = instanceOf(new MonadClass[Set] {
    override def map[A, B](fa: Set[A])(f: A => B): Set[B]          = fa.map(f)
    override def ap[A, B](fa: Set[A])(f: Set[A => B]): Set[B]      = fa.zip(f).map { case (e, f) => f(e) }
    override def pure[A](a: A): Set[A]                             = Set(a)
    override def flatMap[A, B](oa: Set[A])(f: A => Set[B]): Set[B] = oa.flatMap(f)
    override def flatten[A](ma: Set[Set[A]]): Set[A]               = ma.flatten
  })

  implicit def setEq[A: Eq]: Eq[Set[A]] =
    instanceOf(new EqClass[Set[A]] {
      def equal(first: Set[A], second: Set[A]): Boolean = (first.toStream.corresponds(second.toStream)(Eq[A].equal))
    })
}
