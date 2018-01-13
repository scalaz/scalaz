package scalaz
package data

sealed abstract class FFree[F[_], A] {

  def map[B](f: A => B): FFree[F, B] =
    flatMap[B](a => FFree.pure[F, B](f(a)))

  def flatMap[B](k: A => FFree[F, B]): FFree[F, B] =
    this match {
      case FFree.Pure(a) => k(a)
      case FFree.Impure(ff, rs) => FFree.impure(ff, rs.:+(Kleisli.wrapKleisli(k)))
    }
}

object FFree extends FFreeInstances {
  private[FFree] type =>:[F[_], A, B] = Kleisli[FFree[F, ?], A, B]
  private[FFree] type Quiver[F[_], A, B] = ACatenable1[=>:[F, ?, ?], A, B]

  final case class Pure[F[_], A](a: A) extends FFree[F, A]
  final case class Impure[F[_], E, A](ff: FFree[F, E], rs: Quiver[F, E, A]) extends FFree[F, A] {
    type EE = E
    def fa: FFree[F, EE] = ff
    def kk: Quiver[F, E, A] = rs
  }

  def pure[F[_], A](a: A): Pure[F, A] = Pure(a)
  def impure[F[_], E, A](ff: FFree[F, E], rs: Quiver[F, E, A]): FFree[F, A] = Impure(ff, rs)

  private[FFree] def tsingleton[F[_], A, B](f: A => FFree[F, B]): Quiver[F, A, B] =
    ACatenable1.lift[=>:[F, ?, ?], A, B](Kleisli.wrapKleisli[FFree[F, ?], A, B](f))

  private[FFree] def singleK[F[_], A, B](r: =>:[F, A, B]): Quiver[F, A, B] =
    ACatenable1.lift[=>:[F, ?, ?], A, B](r)
}
