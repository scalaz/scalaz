package scalaz

sealed abstract class LeibnizF[A[_], B[_]] {
  def apply[X](a: A[X]): B[X] = subst[({type l[F[_]] = F[X]})#l](a)
  def subst[F[_[_]]](p: F[A]): F[B]
}

object LeibnizF {
  type =~=[A[_], B[_]] = LeibnizF[A, B]

  /** Equality is reflexive */
  implicit def refl[A[_]]: LeibnizF[A, A] = new LeibnizF[A, A] {
    def subst[F[_[_]]](p: F[A]): F[A] = p
  }

  /** Equality is transitive */
  def trans[A[_], B[_], C[_]](
    f: LeibnizF[B, C],
    g: LeibnizF[A, B]
  ): LeibnizF[A, C] =
    f.subst[({type l[X[_]] = LeibnizF[A, X]})#l](g)

  /** Equality is symmetric */
  def symm[A[_], B[_]](
    f: LeibnizF[A, B]
  )  : LeibnizF[B, A] =
    f.subst[({type l[X[_]] = LeibnizF[X, A]})#l](refl)
}
