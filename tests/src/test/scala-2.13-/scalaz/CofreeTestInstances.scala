package scalaz

import scalaz.Cofree.CofreeZip

trait CofreeTestInstances {

  implicit def cofreeEqual[F[_], A](implicit F: Eq1[F], A: Equal[A]): Equal[Cofree[F, A]] =
    Equal.equal{ (a, b) =>
      A.equal(a.head, b.head) && F.eq1(cofreeEqual[F, A]).equal(a.tail, b.tail)
    }

  implicit def cofreeZipEqual[F[_]: Eq1, A: Equal]: Equal[CofreeZip[F, A]] =
    Tag.subst(cofreeEqual[F, A])

  //needed to prevent SOE for testing with equality
  implicit def cofreeOptEquals[A](implicit e: Equal[A]): Equal[Cofree[Option, A]] = new Equal[Cofree[Option, A]] {
    override def equal(a: Cofree[Option, A], b: Cofree[Option, A]): Boolean = {
      def tr(a: Cofree[Option, A], b: Cofree[Option, A]): Boolean =
        (a.tail, b.tail) match {
          case (Some(at), Some(bt)) if (e.equal(a.head, b.head)) => tr(at, bt)
          case (None, None) if (e.equal(a.head, b.head)) => true
          case _ => false
        }
      tr(a,b)
    }
  }
}
