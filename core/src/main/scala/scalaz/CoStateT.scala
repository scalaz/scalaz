package scalaz

sealed trait CoStateT[F[_], A, B] {
  def runT: (F[A => B], A)

  import CoStateT._
  import Isomorphism.<~>

  def run(implicit i: F <~> Id): (A => B, A) = {
    val (k, a) = runT
    (i.to(k), a)
  }

  def putT(implicit F: Functor[F]): A => F[B] =
    a => F.map(runT._1)(k => k(a))

  def put(a: A)(implicit i: F <~> Id): B =
    run._1(a)

  def pos: A =
    runT._2

  def copureT(implicit F: CoPointed[F]): B =
    F.copure(runT._1)(runT._2)

  def copure(implicit i: F <~> Id): B =
    run._1(run._2)

  def map[C](f: B => C)(implicit ftr: Functor[F]): CoStateT[F, A, C] =
    coStateT(mapRunT(k => f compose k))

  def duplicateT(implicit F: CoMonad[F]): CoStateT[F, A, CoStateT[F, A, B]] =
    coStateT((F.cobind(runT._1)(ff => (a: A) => coStateT[F, A, B]((ff, a))), pos))

  def duplicate(implicit i: F <~> Id): CoState[A, CoState[A, B]] =
    coState[A, CoState[A, B]](
      mapRun[A => CoState[A, B]](k => a =>
        coState[A, B]((k, run._2))))

  def cobindT[C](f: CoStateT[F, A, B] => C)(implicit c: CoBind[F]): CoStateT[F, A, C] =
    coStateT((CoBind[F].cobind(runT._1)(ff => (a: A) => f(coStateT[F, A, B]((ff, a)))), pos))

  def cobind[C](f: CoState[A, B] => C)(implicit i: F <~> Id): CoState[A, C] =
    coState[A, C](((a: A) => f(coState[A, B]((run._1, a))), pos))

  private def mapRun[C](f: (A => B) => C)(implicit i: F <~> Id): (C, A) = {
    val (k, a) = run
    (f(k), a)
  }

  private def mapRunT[C](f: (A => B) => C)(implicit F: Functor[F]): (F[C], A) =
    (F.map(runT._1)(f), runT._2)
}

object CoStateT extends CoStateTs {
  def apply[F[_], A, B](r: (F[A => B], A)): CoStateT[F, A, B] =
    coStateT(r)
}

trait CoStateTs {
  type CoState[A, B] =
  CoStateT[Id, A, B]
  // CoState is also known as Store
  type Store[A, B] =
  CoState[A, B]
  // flipped
  type |-->[A, B] =
  CoState[B, A]

  def coStateT[F[_], A, B](r: (F[A => B], A)): CoStateT[F, A, B] = new CoStateT[F, A, B] {
    val runT = r
  }

  def coState[A, B](r: (A => B, A)): CoState[A, B] =
    coStateT[Id, A, B](r._1, r._2)
}