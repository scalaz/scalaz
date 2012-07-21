package scalaz

trait Sorter[F[_]] {
  def sortWith[A](fa: F[A])(lt: (A,A) ⇒ Boolean): F[A] =
    sort(fa)(Order.fromScalaOrdering(scala.math.Ordering.fromLessThan(lt)))
//  def sortWith[A](fa: F[A])(lt: (A,A) ⇒ Ordering): F[A] = sort(fa)(Order.order(lt))
  def sortBy[A,B:Order](fa: F[A])(f: A ⇒ B): F[A] = sort(fa)(Order.orderBy(f))
  def sort[A:Order](fa: F[A]): F[A]
}

object Sorter {
  @inline def apply[F[_]](implicit F: Sorter[F]): Sorter[F] = F
}
