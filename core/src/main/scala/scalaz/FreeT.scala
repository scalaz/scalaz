package scalaz

import FreeT._

object FreeT extends FreeTInstances {
  /** Suspend the computation with the given suspension. */
  private case class Suspend[S[_], M[_], A](a: M[A \/ S[FreeT[S, M, A]]]) extends FreeT[S, M, A]

  /** Call a subroutine and continue with the given function. */
  private case class Gosub[S[_], M[_], B, C](a: FreeT[S, M, C], f: C => FreeT[S, M, B]) extends FreeT[S, M, B]

  /** Return the given value in the free monad. */
  def point[S[_], M[_], A](value: A)(implicit M: Applicative[M]): FreeT[S, M, A] = Suspend(M.point(-\/(value)))

  def tailrecM[S[_], M[_]: Applicative, A, B](f: A => FreeT[S, M, A \/ B])(a: A): FreeT[S, M, B] =
    f(a).flatMap {
      case -\/(a0) => tailrecM(f)(a0)
      case \/-(b) => point[S, M, B](b)
    }

  def liftM[S[_], M[_], A](value: M[A])(implicit M: Functor[M]): FreeT[S, M, A] =
    Suspend(M.map(value)(\/.left))

  /** Suspends a value within a functor in a single step. Monadic unit for a higher-order monad. */
  def liftF[S[_], M[_], A](value: S[A])(implicit S: Functor[S], M: Applicative[M]): FreeT[S, M, A] =
    Suspend(M.point(\/-(S.map(value)(point[S, M, A]))))
}

sealed abstract class FreeT[S[_], M[_], A] {
  final def map[B](f: A => B)(implicit S: Functor[S], M: Functor[M]): FreeT[S, M, B] =
    this match {
      case Suspend(m) => Suspend(M.map(m)(_.bimap(f, S.lift(_.map(f)))))
      case Gosub(a, k) => Gosub(a, (x: Any) => k(x).map(f))
    }

  /** Binds the given continuation to the result of this computation. */
  final def flatMap[B](f: A => FreeT[S, M, B]): FreeT[S, M, B] =
    this match {
      case Gosub(a, k) => Gosub(a, (x: Any) => Gosub(k(x), f))
      case a => Gosub(a, f)
    }

  /** Evaluates a single layer of the free monad **/
  def resume(implicit S: Functor[S], M0: BindRec[M], M1: Applicative[M]): M[A \/ S[FreeT[S, M, A]]] = {
    def go(ft: FreeT[S, M, A]): M[FreeT[S, M, A] \/ (A \/ S[FreeT[S, M, A]])] =
      ft match {
        case Suspend(f) => M0.map(f)(\/.right)
        case Gosub(m0, f0) => m0 match {
          case Suspend(m1) => M0.map(m1) {
            case -\/(a) => -\/(f0(a))
            case \/-(fc) => \/-(\/-(S.map(fc)(_.flatMap(f0))))
          }
          case Gosub(m1, f1) => M1.point(-\/(m1.flatMap(f1(_).flatMap(f0))))
        }
      }

    M0.tailrecM(go)(this)
  }

  /**
    * Runs to completion, using a function that maps the resumption from `S` to a monad `M`.
    */
  def runM(interp: S[FreeT[S, M, A]] => M[FreeT[S, M, A]])(implicit S: Functor[S], M0: BindRec[M], M1: Applicative[M]): M[A] = {
    def runM2(ft: FreeT[S, M, A]): M[FreeT[S, M, A] \/ A] =
      M0.bind(ft.resume) {
        case -\/(a) => M1.point(\/-(a))
        case \/-(fc) => M0.map(interp(fc))(\/.left)
      }

    M0.tailrecM(runM2)(this)
  }
}

sealed abstract class FreeTInstances0 {
  implicit def freeTBind[S[_], M[_]](implicit S0: Functor[S], M0: Functor[M]): Bind[FreeT[S, M, ?]] =
    new FreeTBind[S, M] {
      implicit def S: Functor[S] = S0
      implicit def M: Functor[M] = M0
    }
}

sealed abstract class FreeTInstances extends FreeTInstances0 {
  implicit def freeTMonad[S[_], M[_]](implicit S0: Functor[S], M0: Applicative[M]): Monad[FreeT[S, M, ?]] with BindRec[FreeT[S, M, ?]] =
    new Monad[FreeT[S, M, ?]] with BindRec[FreeT[S, M, ?]] with FreeTBind[S, M] {
      implicit def S: Functor[S] = S0
      implicit def M: Functor[M] = M0

      def point[A](a: => A): FreeT[S, M, A] = FreeT.point[S, M, A](a)

      def tailrecM[A, B](f: A => FreeT[S, M, A \/ B])(a: A): FreeT[S, M, B] = FreeT.tailrecM(f)(a)
    }
}

private trait FreeTBind[S[_], M[_]] extends Bind[FreeT[S, M, ?]] {
  implicit def S: Functor[S]
  implicit def M: Functor[M]

  override def map[A, B](fa: FreeT[S, M, A])(f: A => B): FreeT[S, M, B] = fa.map(f)
  def bind[A, B](fa: FreeT[S, M, A])(f: A => FreeT[S, M, B]): FreeT[S, M, B] = fa.flatMap(f)
}
