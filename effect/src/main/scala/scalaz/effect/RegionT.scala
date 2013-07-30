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
sealed trait RegionT[S, P[_], A] {
  def value: Kleisli[P, IORef[List[RefCountedFinalizer]], A]

  def runT(r: IORef[List[RefCountedFinalizer]]): P[A] =
    value.run(r)
}

object RegionT extends RegionTInstances with RegionTFunctions {
  def apply[S, P[_], A](k: Kleisli[P, IORef[List[RefCountedFinalizer]], A]): RegionT[S, P, A] = new RegionT[S, P, A] {
    val value = k
  }
}

sealed abstract class RegionTInstances1 {
  implicit def RegionTLiftIO[S, M[_]](implicit M: LiftIO[M]): LiftIO[({type λ[α] = RegionT[S, M, α]})#λ] = new RegionTLiftIO[S, M] {
    implicit def L = M
  }

  implicit def RegionTMonad[S, M[_]](implicit M0: Monad[M]): Monad[({type λ[α] = RegionT[S, M, α]})#λ] = new RegionTMonad[S, M] {
    implicit def M = M0
  }
}

sealed abstract class RegionTInstances extends RegionTInstances1 {
}

trait RegionTFunctions {
  def regionT[S, P[_], A](k: Kleisli[P, IORef[List[RefCountedFinalizer]], A]): RegionT[S, P, A] = RegionT(k)
}

trait RegionTMonad[S, M[_]] extends Monad[({type λ[α] = RegionT[S, M, α]})#λ] {
  implicit def M: Monad[M]

  def point[A](a: => A): RegionT[S, M, A] = RegionT(kleisli(s => M.point(a)))
  def bind[A, B](fa: RegionT[S, M, A])(f: A => RegionT[S, M, B]): RegionT[S, M, B] =
    RegionT(kleisli(s => M.bind(fa.value.run(s))((a: A) => f(a).value.run(s))))
}

trait RegionTLiftIO[S, M[_]] extends LiftIO[({type λ[α] = RegionT[S, M, α]})#λ] {
  implicit def L: LiftIO[M]
  
  def liftIO[A](ioa: IO[A]) = RegionT.regionT(kleisli(_ => L.liftIO(ioa)))
}

