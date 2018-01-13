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

//  final def foldMap[M[_]](α: F ~> M)(implicit M: Monad[M]): M[A] = ???
//
//  final def hoist[G[_]](α: F ~> FFree[G, ?]): FFree[G, A] =
//    foldMap(α)(FFree.monad)

  final def foldFree[B](la: F[FFree[F, A]] => B)(ra: A => B): B =
    runFree.fold(la)(ra)

  final def runFree: F[FFree[F, A]] \/ A = ???

//  @tailrec
  private[FFree] final def step: FFree[F, A] = ???
}

object FFree extends FFreeInstances {
  private[FFree] type =>:[F[_], A, B] = Kleisli[FFree[F, ?], A, B]
  private[FFree] type Quiver[F[_], A, B] = ACatenable1[=>:[F, ?, ?], A, B]

  final case class Pure[F[_], A] private(a: A) extends FFree[F, A]
  final case class Impure[F[_], E, A] private(f: FFree[F, E], q: Quiver[F, E, A]) extends FFree[F, A] {
    type EE = E
    def ff: FFree[F, EE] = f
    def qq: Quiver[F, E, A] = q
  }

  def pure[F[_], A](a: A): Pure[F, A] = Pure(a)
  def impure[F[_], E, A](ff: FFree[F, E], rs: Quiver[F, E, A]): FFree[F, A] = Impure(ff, rs)

  def defer[F[_], A](f: FFree[F, A]): FFree[F, A] = pure(()) flatMap (_ => f)


  private[FFree] def ksingleton[F[_], A, B](f: A => FFree[F, B]): Quiver[F, A, B] =
    ACatenable1.lift[=>:[F, ?, ?], A, B](Kleisli.wrapKleisli[FFree[F, ?], A, B](f))

  private[FFree] def singleK[F[_], A, B](r: =>:[F, A, B]): Quiver[F, A, B] =
    ACatenable1.lift[=>:[F, ?, ?], A, B](r)
}
