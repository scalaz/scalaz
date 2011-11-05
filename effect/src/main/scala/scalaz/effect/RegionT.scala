package scalaz
package effect

import Kleisli._

// An implementation of "Lightweight Monadic Regions" by Kiselyov and Shan
// http://okmij.org/ftp/Haskell/regions.html#light-weight
// Based on a Haskell library by Bas van Dijk

/**
 * A monad transformer in which scarce resources can be opened. When the region
 * terminates, all opened resources will be closed automatically. It's a type error
 * to return an opened resorce from the region, and no I/O with closed
 * resources is possible.
 */
sealed trait RegionT[S, P[_], A] {
  def value: Kleisli[P, IORef[List[RefCountedFinalizer]], A]

  def runT(r: IORef[List[RefCountedFinalizer]]): P[A] =
    value.run(r)

/*  def run(r: IORef[List[RefCountedFinalizer]])(implicit i: P =~~= Identity): A =
    runT(r)*/
}

object RegionT extends RegionTs

trait RegionTs {
  type Region[S, A] =
  RegionT[S, Identity, A]

  def regionT[S, P[_], A](k: Kleisli[P, IORef[List[RefCountedFinalizer]], A]): RegionT[S, P, A] = new RegionT[S, P, A] {
    val value = k
  }

  implicit def RegionTMonad[S, M[_]](implicit M: Monad[M]) = new Monad[({type λ[α] = RegionT[S, M, α]})#λ] {
    def point[A](a: => A): RegionT[S, M, A] = regionT(kleisli(s => M.point(a)))
    def bind[A, B](fa: RegionT[S, M, A])(f: (A) => RegionT[S, M, B]): RegionT[S, M, B] =
       regionT(kleisli(s => M.bind(fa.value.run(s))((a: A) => f(a).value.run(s))))
  }
}
