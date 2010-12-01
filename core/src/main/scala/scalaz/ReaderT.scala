package scalaz

import Scalaz._

/** Reader monad transformer
  * Can be used to add environment reading capabilities to other monads
  * Note: ReaderT[M,R,A] is isomorphic to Kleisli[M,R,A].
  */
case class ReaderT[M[_], R, A](value: R => M[A]) extends NewType[R => M[A]]

object ReaderT {

  implicit def readerTPure[M[_], R](implicit m: Pure[M]) =
    new Pure[({type λ[α]=ReaderT[M, R, α]})#λ] {
      def pure[A](a: => A) = ReaderT((r: R) => m.pure(a))
    }

  implicit def readerTBind[M[_]:Bind, R] =
    new Bind[({type λ[α]=ReaderT[M, R, α]})#λ] {
      def bind[A,B](m: ReaderT[M, R, A], k: A => ReaderT[M, R, B]) =
        ReaderT((r: R) => m.value(r) >>= (a => k(a).value(r)))
    }

}
