package scalaz

sealed abstract class LiskovF[-A[_], +B[_]] {
  def substCo[F[+_[_]]](p: F[A]): F[B]
  def substCt[F[-_[_]]](p: F[B]): F[A]

  def apply[X](a: A[X]): B[X] = substCo[({type l[F[_]] = F[X]})#l](a)
}

object LiskovF {
  type <~~<[-A[_], +B[_]] = LiskovF[A, B]
  type >~~>[+A[_], -B[_]] = LiskovF[B, A]

  implicit def refl[A[_]]: A <~~< A = new (A <~~< A) {
    def substCo[F[+ _[_]]](p: F[A]) = p
    def substCt[F[- _[_]]](p: F[A]) = p
  }

}
