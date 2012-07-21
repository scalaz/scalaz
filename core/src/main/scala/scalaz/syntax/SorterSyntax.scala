package scalaz
package syntax

trait SorterOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Sorter[F]

  final def sortWith(lt: (A,A) ⇒ Boolean): F[A] = F.sortWith(self)(lt)
//  final def sortWith(lt: (A,A) ⇒ Ordering): F[A] = F.sortWith(self)(lt)
  final def sortBy[B:Order](f: A ⇒ B): F[A] = F.sortBy(self)(f)
  final def sorted(implicit ord: Order[A]): F[A] = F.sort(self)
}

trait ToSorterOps {
  implicit def ToSorterOps[F[_],A](fa: F[A])(implicit F0: Sorter[F]): SorterOps[F,A] =
    new SorterOps[F,A] { def self = fa ; implicit def F: Sorter[F] = F0 }
}

trait SorterSyntax[F[_]] {
  implicit def ToSorterOps[A](fa: F[A])(implicit F0: Sorter[F]): SorterOps[F,A] =
    new SorterOps[F,A] { def self = fa ; implicit def F: Sorter[F] = F0 }
}
