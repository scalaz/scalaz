package scalaz

import Kleisli._

sealed trait ReaderWriterStateT[R, W, S, F[_], A] {
  def apply: R => S => F[(A, S, W)]

  def state(r: R)(implicit ftr: Functor[F]): StateT[S, F, A] =
    StateT.stateT((s: S) => ftr.fmap((asw: (A, S, W)) => (asw._1, asw._2))(apply(r)(s)))

  def rws(implicit ftr: Functor[F]): ReaderT[R, ({type λ[α] = StateT[S, ({type λ[α] = WriterT[W, F, α]})#λ, α]})#λ, A] =
    Kleisli.kleisli[R, ({type λ[α] = StateT[S, ({type λ[α] = WriterT[W, F, α]})#λ, α]})#λ, A](r =>
      StateT.stateT[S, ({type λ[α] = WriterT[W, F, α]})#λ, A](s =>
        WriterT.writerT[W, F, (A, S)](implicitly[Functor[F]].fmap((asw: (A, S, W)) => (asw._3, (asw._1, asw._2)))(apply(r)(s)))))
}

object ReaderWriterStateT extends ReaderWriterStateTs

trait ReaderWriterStateTs {
  type ReaderWriterState[R, W, S, A] =
  ReaderWriterStateT[R, W, S, Identity, A]

  type RWST[R, W, S, F[_], A] =
  ReaderWriterStateT[R, W, S, F, A]

  type RWS[R, W, S, A] =
  ReaderWriterState[R, W, S, A]
}