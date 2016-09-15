package scalaz
package effect

import Kleisli._

// An implementation of "Lightweight Monadic Regions" by Kiselyov and Shan
// http://okmij.org/ftp/Haskell/regions.html#light-weight
// Based on a Haskell library by Bas van Dijk

/**
 * A monad transformer in which scarce resources can be opened. When the region
 * terminates, all opened resources will be closed automatically. It's a type error
 * to return an opened resource from the region, and no I/O with closed
 * resources is possible.
 */
sealed abstract class RegionT[S, P[_], A] {
  def value: Kleisli[P, IORef[List[RefCountedFinalizer]], A]

  def runT(r: IORef[List[RefCountedFinalizer]]): P[A] =
    value.run(r)

  def mapT[Q[_], B](f: P[A] => Q[B]): RegionT[S, Q, B] =
    RegionT(Kleisli(f compose runT))
}

object RegionT extends RegionTInstances {
  def apply[S, P[_], A](k: Kleisli[P, IORef[List[RefCountedFinalizer]], A]): RegionT[S, P, A] = new RegionT[S, P, A] {
    val value = k
  }

  def regionT[S, P[_], A](k: Kleisli[P, IORef[List[RefCountedFinalizer]], A]): RegionT[S, P, A] = RegionT(k)
}

sealed abstract class RegionTInstances1 {
  implicit def RegionTLiftIO[S, M[_]](implicit M: LiftIO[M]): LiftIO[RegionT[S, M, ?]] = new RegionTLiftIO[S, M] {
    implicit def L = M
  }

  implicit def RegionTMonad[S, M[_]](implicit M0: Monad[M]): Monad[RegionT[S, M, ?]] = new RegionTMonad[S, M] {
    implicit def M = M0
  }

  implicit def RegionTHoist[S]: Hoist[λ[(α[_], β) => RegionT[S, α, β]]] = new Hoist[λ[(α[_], β) => RegionT[S, α, β]]] {
    def hoist[M[_]: Monad, N[_]](f: M ~> N) =
      λ[RegionT[S, M, ?] ~> RegionT[S, N, ?]](fa =>
        RegionT(Kleisli(r => f.apply(fa.runT(r))))
      )

    override def liftM[M[_]: Monad, B](a: M[B]) =
      RegionT(Kleisli(r => a))

    override implicit def apply[M[_]: Monad] =
      RegionTMonad[S, M]
  }

}

sealed abstract class RegionTInstances extends RegionTInstances1 {
}

trait RegionTMonad[S, M[_]] extends Monad[RegionT[S, M, ?]] {
  implicit def M: Monad[M]

  def point[A](a: => A): RegionT[S, M, A] = RegionT(kleisli(s => M.point(a)))
  def bind[A, B](fa: RegionT[S, M, A])(f: A => RegionT[S, M, B]): RegionT[S, M, B] =
    RegionT(kleisli(s => M.bind(fa.value.run(s))((a: A) => f(a).value.run(s))))
}

trait RegionTLiftIO[S, M[_]] extends LiftIO[RegionT[S, M, ?]] {
  implicit def L: LiftIO[M]

  def liftIO[A](ioa: IO[A]) = RegionT.regionT(kleisli(_ => L.liftIO(ioa)))
}

