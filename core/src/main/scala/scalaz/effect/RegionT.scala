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
  val value: Kleisli[IORef[List[RefCountedFinalizer]], P, A]

  import =~~=._

  def runT(r: IORef[List[RefCountedFinalizer]]): P[A] =
    value.run(r)

  def run(r: IORef[List[RefCountedFinalizer]])(implicit i: P =~~= Identity): A =
    runT(r)
}

object RegionT extends RegionTs

trait RegionTs {
  type Region[S, A] =
  RegionT[S, Identity, A]

  def regionT[S, P[_], A](k: Kleisli[IORef[List[RefCountedFinalizer]], P, A]): RegionT[S, P, A] = new RegionT[S, P, A] {
    val value = k
  }

  implicit def RegionTBind[S, M[_]](implicit b: Bind[M]): Bind[({type λ[α] = RegionT[S, M, α]})#λ] =
    new Bind[({type λ[α] = RegionT[S, M, α]})#λ] {
      def bind[A, B](f: A => RegionT[S, M, B]) =
        (m: RegionT[S, M, A]) => regionT(kleisli(s => b.bind((a: A) => f(a).value.run(s))(m.value.run(s))))
    }

  implicit def RegionTPointed[S, M[_]](implicit p: Pointed[M]): Pointed[({type λ[α] = RegionT[S, M, α]})#λ] =
    new Pointed[({type λ[α] = RegionT[S, M, α]})#λ] {
      def point[A](a: => A): RegionT[S, M, A] = regionT(kleisli(s => p.point(a)))
    }

  implicit def RegionTMonad[S, M[_]](implicit m: Monad[M]): Monad[({type λ[α] = RegionT[S, M, α]})#λ] = {
    implicit val b = m.bind
    implicit val p = m.pointed
    Monad.monadBP[({type λ[α] = RegionT[S, M, α]})#λ]
  }
}
