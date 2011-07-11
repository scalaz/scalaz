package scalaz

import scala.Predef._

sealed trait CoStateT[A, F[_], B] {
  val runT: (F[A => B], A)

  import CoStateT._

  def *->* : (({type λ[α] = CoStateT[A, F, α]})#λ *->* B) =
    scalaz.*->*.!**->**![({type λ[α] = CoStateT[A, F, α]})#λ, B](this)

  def *->*->* : *->*->*[A, ({type λ[α, β] = CoStateT[α, F, β]})#λ, B] =
    scalaz.*->*->*.!**->**->**![A, ({type λ[α, β] = CoStateT[α, F, β]})#λ, B](this)

  private def mapRunT[C](f: (A => B) => C)(implicit ftr: Functor[F]): (F[C], A) =
    (ftr.fmap((z: A => B) => f(z))(runT._1), runT._2)

  private def mapRun[C](f: (A => B) => C)(implicit i: F[A => B] =:= Identity[A => B]): (C, A) = {
    val (k, a) = run
    (f(k), a)
  }

  def run(implicit i: F[A => B] =:= Identity[A => B]): (A => B, A) = {
    val (k, a) = runT
    (k.value, a)
  }

  def putT(implicit ftr: Functor[F]): A => F[B] =
    a => ftr.fmap((k: A => B) => k(a))(runT._1)

  def put(implicit i: F[A => B] =:= Identity[A => B]): A => B =
    run._1

  def pos: A =
    runT._2

  def copointT(implicit p: CoPointed[F]): B =
    p.coPoint(runT._1)(runT._2)

  def copoint(implicit i: F[A => B] =:= Identity[A => B]): B =
    run._1(run._2)

  def map[C](f: B => C)(implicit ftr: Functor[F]): CoStateT[A, F, C] =
    coStateT[A, F, C](mapRunT(k => f compose k))

  def duplicateT(implicit p: CoBind[F]): CoStateT[A, F, CoStateT[A, F, B]] =
    coStateT[A, F, CoStateT[A, F, B]]((
        p.coBind((ff: F[A => B]) => (a: A) => coStateT[A, F, B]((ff, a)))(runT._1)
        , pos))

  def duplicate(implicit i: F[A => B] =:= Identity[A => B]): CoState[A, CoState[A, B]] =
    coState[A, CoState[A, B]](
      mapRun[A => CoState[A, B]](k => a =>
        coState[A, B]((k, run._2))))

  def cobindT[C](f: CoStateT[A, F, B] => C)(implicit c: CoBind[F]): CoStateT[A, F, C] =
    coStateT[A, F, C]((
        implicitly[CoBind[F]].coBind((ff: F[A => B]) => (a: A) =>
          f(coStateT[A, F, B]((ff, a)))
        )(runT._1)
        , pos
        ))

  def cobind[C](f: CoState[A, B] => C)(implicit i: F[A => B] =:= Identity[A => B]): CoState[A, C] =
    coState[A, C]((
        (a: A) => f(coState[A, B]((run._1, a)))
        , pos
        ))
}

object CoStateT extends CoStateTs {
  def apply[A, F[_], B](r: (F[A => B], A)): CoStateT[A, F, B] =
    coStateT(r)
}

trait CoStateTs {
  type CoState[A, B] =
  CoStateT[A, Identity, B]
  type CostateT[A, F[_], B] =
  CoStateT[A, F, B]
  type Costate[A, B] =
  CoState[A, B]
  // CoState is also known as Store
  type Store[A, B] =
  CoState[A, B]
  // flipped
  type |-->[A, B] =
  CoState[B, A]

  type PartialApplyCoState[A] =
  PartialApply1Of2[CoState, A]

  def coStateT[A, F[_], B](r: (F[A => B], A)): CoStateT[A, F, B] = new CoStateT[A, F, B] {
    val runT = r
  }

  def coState[A, B](r: (A => B, A)): CoState[A, B] =
    coStateT[A, Identity, B](Identity.id(r._1), r._2)

  def costate[A, B](r: (A => B, A)): CoState[A, B] =
    coState(r)

  def store[A, B](r: (A => B, A)): Store[A, B] =
    coState(r)

}