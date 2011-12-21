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

object CoStateT extends CoStateTFunctions with CoStateTInstances {
  def apply[F[_], A, B](r: (F[A => B], A)): CoStateT[F, A, B] =
    coStateT(r)
}

trait CoStateTFunctions {
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

trait CoStateTInstances2 {
  implicit def coStateTFunctor[F[_], A](implicit F0: Functor[F]) = new CoStateTFunctor[F, A] {
    implicit def F: Functor[F] = F0
  }
}
trait CoStateTInstances1 extends CoStateTInstances2 {
  implicit def coStateTCoPointed[F[_], A](implicit F0: CoPointed[F]) = new CoStateTCoPointed[F, A] {
    implicit def F: CoPointed[F] = F0
  }
}
trait CoStateTInstances0 extends CoStateTInstances1 {
  implicit def coStateTCoBind[F[_], A](implicit F0: CoBind[F]) = new CoStateTCoBind[F, A] {
    implicit def F: CoBind[F] = F0
  }
}

trait CoStateTInstances extends CoStateTInstances0 {
  implicit def coStateTCoMonad[F[_], A](implicit F0: CoMonad[F]) = new CoStateTCoMonad[F, A] {
    implicit def F: CoMonad[F] = F0
  }
}

trait CoStateTFunctor[F[_], A0] extends Functor[({type λ[α]=CoStateT[F, A0, α]})#λ]{
  implicit def F: Functor[F]
  def map[A, B](fa: CoStateT[F, A0, A])(f: (A) => B): CoStateT[F, A0, B] = fa map f
}

trait CoStateTCoPointed[F[_], A0] extends CoPointed[({type λ[α]=CoStateT[F, A0, α]})#λ] with CoStateTFunctor[F, A0] {
  implicit def F: CoPointed[F]
  def copoint[A](p: CoStateT[F, A0, A]) = p.copoint
}

trait CoStateTCoBind[F[_], A0] extends CoBind[({type λ[α]=CoStateT[F, A0, α]})#λ] {
  implicit def F: CoBind[F]
  def cobind[A, B](fa: CoStateT[F, A0, A])(f: (CoStateT[F, A0, A]) => B) = fa cobind f
}

trait CoStateTCoMonad[F[_], A0] extends CoMonad[({type λ[α]=CoStateT[F, A0, α]})#λ] with CoStateTCoBind[F, A0] with CoStateTCoPointed[F, A0]{
  implicit def F: CoMonad[F]
  def cojoin[A](a: CoStateT[F, A0, A]) = a.duplicate
}
