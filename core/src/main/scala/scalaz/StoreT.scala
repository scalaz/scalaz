package scalaz

import Id._

/**
 * @see [[scalaz.Lens]]
 */
sealed trait StoreT[F[+_], A, +B] {
  def run: (F[A => B], A)

  import StoreT._
  import BijectionT._

  def xmap[X](f: A => X)(g: X => A)(implicit F: Functor[F]): StoreT[F, X, B] =
    storeT(F.map(set)(_ compose g), f(pos))

  def bmap[X](b: Bijection[A, X])(implicit F: Functor[F]): StoreT[F, X, B] =
    xmap(b to _)(b from _)

  def put(a: A)(implicit F: Functor[F]): F[B] =
    F.map(run._1)(_(a))

  def puts(f: A => A)(implicit F: Functor[F]): F[B] =
    put(f(pos))

  def set: F[A => B] =
    run._1

  def pos: A =
    run._2

  def copoint(implicit F: Copointed[F]): B =
    F.copoint(run._1)(run._2)

  def map[C](f: B => C)(implicit ftr: Functor[F]): StoreT[F, A, C] =
    storeT(mapRunT(k => f compose k))

  def duplicate(implicit F: Comonad[F]): StoreT[F, A, StoreT[F, A, B]] =
    storeT((F.cobind(run._1)(ff => (a: A) => storeT[F, A, B]((ff, a))), pos))

  def cobind[C](f: StoreT[F, A, B] => C)(implicit c: Cobind[F]): StoreT[F, A, C] =
    storeT((Cobind[F].cobind(run._1)(ff => (a: A) => f(storeT[F, A, B]((ff, a)))), pos))

  /** Two disjoint lenses can be paired */
  def product[C, D](that: StoreT[F, C, D])(implicit M: Bind[F]): StoreT[F, (A, C), (B, D)] =
    StoreT(M.bind(set) { s => M.map(that.set)(t => { (ac: (A, C)) => (s(ac._1), t(ac._2))})}, (pos, that.pos))

  /** alias for `product` */
  def ***[C, D](that: StoreT[F, C, D])(implicit M: Bind[F]): StoreT[F, (A, C), (B, D)] = product(that)

  private def mapRunT[C](f: (A => B) => C)(implicit F: Functor[F]): (F[C], A) =
    (F.map(run._1)(f), run._2)
}

object StoreT extends StoreTFunctions with StoreTInstances {
  def apply[F[+_], A, B](r: (F[A => B], A)): StoreT[F, A, B] =
    storeT(r)
}

trait StoreTFunctions {

  def storeT[F[+_], A, B](r: (F[A => B], A)): StoreT[F, A, B] = new StoreT[F, A, B] {
    val run = r
  }

  def store[A, B](a: A)(f: A => B): Store[A, B] =
    storeT[Id, A, B](f -> a)
}

trait StoreTInstances2 {
  implicit def storeTFunctor[F[+_], A](implicit F0: Functor[F]) = new StoreTFunctor[F, A] {
    implicit def F: Functor[F] = F0
  }
}
trait StoreTInstances1 extends StoreTInstances2 {
  implicit def storeTCopointed[F[+_], A](implicit F0: Copointed[F]) = new StoreTCopointed[F, A] {
    implicit def F: Copointed[F] = F0
  }
}
trait StoreTInstances0 extends StoreTInstances1 {
  implicit def storeTCobind[F[+_], A](implicit F0: Cobind[F]) = new StoreTCobind[F, A] {
    implicit def F: Cobind[F] = F0
  }
}

trait StoreTInstances extends StoreTInstances0 {
  implicit def storeTComonad[F[+_], A](implicit F0: Comonad[F]) = new StoreTComonad[F, A] {
    implicit def F: Comonad[F] = F0
  }
}

private[scalaz] trait StoreTFunctor[F[+_], A0] extends Functor[({type λ[+α]=StoreT[F, A0, α]})#λ]{
  implicit def F: Functor[F]
  override def map[A, B](fa: StoreT[F, A0, A])(f: (A) => B): StoreT[F, A0, B] = fa map f
}

private[scalaz] trait StoreTCopointed[F[+_], A0] extends Copointed[({type λ[+α]=StoreT[F, A0, α]})#λ] with StoreTFunctor[F, A0] {
  implicit def F: Copointed[F]
  def copoint[A](p: StoreT[F, A0, A]) = p.copoint
}

private[scalaz] trait StoreTCobind[F[+_], A0] extends Cobind[({type λ[+α]=StoreT[F, A0, α]})#λ] {
  implicit def F: Cobind[F]
  def cobind[A, B](fa: StoreT[F, A0, A])(f: (StoreT[F, A0, A]) => B) = fa cobind f
  override def map[A, B](fa: StoreT[F, A0, A])(f: (A) => B): StoreT[F, A0, B] = fa map f
}

private[scalaz] trait StoreTComonad[F[+_], A0] extends Comonad[({type λ[+α]=StoreT[F, A0, α]})#λ] with StoreTCobind[F, A0] with StoreTCopointed[F, A0]{
  implicit def F: Comonad[F]
  def cojoin[A](a: StoreT[F, A0, A]) = a.duplicate
}

private[scalaz] trait StoreTCohoist[S] extends Cohoist[({type f[g[+_], +a] = StoreT[g, S, a]})#f] {
  def lower[G[+_] : Cobind, A](a: StoreT[G, S, A]) =
    Cobind[G].map(a.run._1)((z: S => A) => z(a.run._2))

  def cohoist[M[+_], N[+_]: Comonad](f: M ~> N) =
    new (({type f[+x] = StoreT[M, S, x]})#f ~> ({type f[+x] = StoreT[N, S, x]})#f) {
      def apply[A](c: StoreT[M, S, A]) = {
        val r = c.run
        StoreT(f(r._1), r._2)
      }
    }
}
