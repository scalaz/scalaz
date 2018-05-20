package scalaz
package ct

trait ApplicativeClass[F[_]] extends ApplyClass[F] {
  def pure[A](a: A): F[A]
}

object ApplicativeClass {

  trait DeriveMap[F[_]] extends ApplicativeClass[F] with MonadClass.Alt[DeriveMap[F]] {
    final override def map[A, B](ma: F[A])(f: (A) => B): F[B] = ap(ma)(pure(f))
  }

}
