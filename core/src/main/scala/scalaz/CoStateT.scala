package scalaz

sealed trait CoStateT[F[_], A, B] {
  def run: (F[A => B], A)

  import CoStateT._

  def put(a: A)(implicit F: Functor[F]): F[B] =
    F.map(run._1)(_(a))

  def pos: A =
    run._2

  def copoint(implicit F: CoPointed[F]): B =
    F.copoint(run._1)(run._2)

  def map[C](f: B => C)(implicit ftr: Functor[F]): CoStateT[F, A, C] =
    coStateT(mapRunT(k => f compose k))

  def duplicate(implicit F: CoMonad[F]): CoStateT[F, A, CoStateT[F, A, B]] =
    coStateT((F.cobind(run._1)(ff => (a: A) => coStateT[F, A, B]((ff, a))), pos))

  def cobind[C](f: CoStateT[F, A, B] => C)(implicit c: CoBind[F]): CoStateT[F, A, C] =
    coStateT((CoBind[F].cobind(run._1)(ff => (a: A) => f(coStateT[F, A, B]((ff, a)))), pos))

  private def mapRunT[C](f: (A => B) => C)(implicit F: Functor[F]): (F[C], A) =
    (F.map(run._1)(f), run._2)
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
    val run = r
  }

  def coState[A, B](r: (A => B, A)): CoState[A, B] =
    coStateT[Id, A, B](r._1, r._2)
}