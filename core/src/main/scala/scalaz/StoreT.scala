package scalaz

import Id._

/**
 * @see [[scalaz.Lens]]
 */
sealed trait IndexedStoreT[F[+_], +I, -A, +B] {
  def run: (F[A => B], I)

  import StoreT._
  import BijectionT._

  def xmap[X1, X2](f: I => X1)(g: X2 => A)(implicit F: Functor[F]): IndexedStoreT[F, X1, X2, B] =
    indexedStoreT((F.map(set)(_ compose g), f(pos)))

  def imap[X](f: I => X): IndexedStoreT[F, X, A, B] =
    indexedStoreT((set, f(pos)))

  def contramap[X](g: X => A)(implicit F: Functor[F]) =
    indexedStoreT((F.map(set)(_ compose g), pos))

  def bimap[X, Y](f: I => X, g: B => Y)(implicit F: Functor[F]): IndexedStoreT[F, X, A, Y] =
    indexedStoreT((F.map(set)(g compose _), f(pos)))

  def bmap[X](b: Bijection[I, X])(implicit F: Functor[F], ev: A <:< I): StoreT[F, X, B] =
    xmap(ev compose (b to _))(b from _)

  def put(a: A)(implicit F: Functor[F]): F[B] =
    F.map(run._1)(_(a))

  def puts(f: I => A)(implicit F: Functor[F]): F[B] =
    put(f(pos))

  def set: F[A => B] =
    run._1

  def pos: I =
    run._2

  def copoint(implicit F: Copointed[F], ev: A <:< I): B =
    F.copoint(run._1)(run._2)

  def map[C](f: B => C)(implicit ftr: Functor[F]): IndexedStoreT[F, I, A, C] =
    indexedStoreT(mapRunT(k => f compose k))

  def duplicate[J](implicit F: Comonad[F]): IndexedStoreT[F, I, J, IndexedStoreT[F, J, A, B]] =
   indexedStoreT((F.cobind(run._1)(ff => (a: J) => indexedStoreT[F, J, A, B]((ff, a))), pos))

  def cobind[K, C](f: IndexedStoreT[F, A, K, B] => C)(implicit c: Cobind[F]): IndexedStoreT[F, I, K, C] =
    indexedStoreT((Cobind[F].cobind(run._1)(ff => (a: I) => f(indexedStoreT[F, I, K, B]((ff, a)))), pos))

  /** Two disjoint lenses can be paired */
  def product[J, C, D](that: IndexedStoreT[F, J, C, D])(implicit M: Bind[F]): IndexedStoreT[F, (I, J), (A, C), (B, D)] =
    IndexedStoreT(M.bind(set) { s => M.map(that.set)(t => { (ac: (A, C)) => (s(ac._1), t(ac._2))})}, (pos, that.pos))

  /** alias for `product` */
  def ***[J, C, D](that: IndexedStoreT[F, J, C, D])(implicit M: Bind[F]): IndexedStoreT[F, (I, J), (A, C), (B, D)] = product(that)

  private def mapRunT[C](f: (A => B) => C)(implicit F: Functor[F]): (F[C], I) =
    (F.map(run._1)(f), run._2)
}

object IndexedStoreT extends StoreTFunctions with StoreTInstances {
  def apply[F[+_], I, A, B](r: (F[A => B], I)): IndexedStoreT[F, I, A, B] =
    indexedStoreT(r)
}

trait IndexedStoreTFunctions {

  def indexedStoreT[F[+_], I, A, B](r: (F[A => B], I)): IndexedStoreT[F, I, A, B] = new IndexedStoreT[F, I, A, B] {
    val run = r
  }

  def indexedStore[I, A, B](i: I)(f: A => B): IndexedStore[I, A, B] =
    indexedStoreT[Id, I, A, B](f -> i)
}

trait StoreTFunctions extends IndexedStoreTFunctions {

  def storeT[F[+_], A, B](r: (F[A => B], A)): StoreT[F, A, B] = 
    indexedStoreT[F, A, A, B](r)

  def store[A, B](a: A)(f: A => B): Store[A, B] =
    storeT[Id, A, B](f -> a)
}
trait IndexedStoreTInstances1 {
  implicit def indexedStoreTBifunctor[F[+_], A](implicit F0: Functor[F]) = new IndexedStoreTBifunctor[F, A] {
    implicit def F: Functor[F] = F0
  }
}
trait IndexedStoreTInstances0 extends IndexedStoreTInstances1 {
  implicit def indexedStoreTFunctor_I[F[+_], A, B](implicit F0: Functor[F]) =
    indexedStoreTBifunctor[F, A].leftFunctor[B]
}
trait IndexedStoreTInstances extends IndexedStoreTInstances0 {
  implicit def indexedStoreTFunctor_A[F[+_], I, A](implicit F0: Functor[F]) =
    indexedStoreTBifunctor[F, A].rightFunctor[I]
}
trait StoreTInstances2 extends IndexedStoreTInstances {
  implicit def storeTCopointed[F[+_], A](implicit F0: Copointed[F]) = new StoreTCopointed[F, A] {
    implicit def F: Copointed[F] = F0
  }
}
trait StoreTInstances1 extends StoreTInstances2 {
  implicit def storeTCobind[F[+_], A](implicit F0: Cobind[F]) = new StoreTCobind[F, A] {
    implicit def F: Cobind[F] = F0
  }
}
trait StoreTInstances0 extends StoreTInstances1 {
  implicit def storeTComonad[F[+_], A](implicit F0: Comonad[F]) = new StoreTComonad[F, A] {
    implicit def F: Comonad[F] = F0
  }
}
trait StoreTInstances extends StoreTInstances0 {
  implicit def storeTCohoist[S]: Cohoist[({type f[g[+_], +a] = StoreT[g, S, a]})#f] = new StoreTCohoist[S] {}
}

trait IndexedStoreTBifunctor[F[+_], A0] extends Bifunctor[({type λ[+α, +β]=IndexedStoreT[F, α, A0, β]})#λ]{
  implicit def F: Functor[F]
  override def bimap[A, B, C, D](fab: IndexedStoreT[F, A, A0, B])(f: A => C, g: B => D): IndexedStoreT[F, C, A0, D] = fab bimap (f, g)
}

trait StoreTCopointed[F[+_], A0] extends Copointed[({type λ[+α]=StoreT[F, A0, α]})#λ] {
  implicit def F: Copointed[F]
  def copoint[A](p: StoreT[F, A0, A]) = p.copoint
  override def map[A, B](fa: StoreT[F, A0, A])(f: (A) => B): StoreT[F, A0, B] = fa map f
}

trait StoreTCobind[F[+_], A0] extends Cobind[({type λ[+α]=StoreT[F, A0, α]})#λ] {
  implicit def F: Cobind[F]
  def cobind[A, B](fa: StoreT[F, A0, A])(f: (StoreT[F, A0, A]) => B) = fa cobind f
  override def map[A, B](fa: StoreT[F, A0, A])(f: (A) => B): StoreT[F, A0, B] = fa map f
}

trait StoreTComonad[F[+_], A0] extends Comonad[({type λ[+α]=StoreT[F, A0, α]})#λ] with StoreTCobind[F, A0] with StoreTCopointed[F, A0]{
  implicit def F: Comonad[F]
  def cojoin[A](a: StoreT[F, A0, A]) = a.duplicate
}

trait StoreTCohoist[S] extends Cohoist[({type f[g[+_], +a] = StoreT[g, S, a]})#f] {
  def lower[G[+_] : Cobind, A](a: StoreT[G, S, A]) =
    Cobind[G].map(a.run._1)((z: S => A) => z(a.run._2))

  def cohoist[M[+_], N[+_]: Comonad](f: M ~> N) =
    new (({type f[+x] = StoreT[M, S, x]})#f ~> ({type f[+x] = StoreT[N, S, x]})#f) {
      def apply[A](c: StoreT[M, S, A]) = {
        val r = c.run
        StoreT((f(r._1), r._2))
      }
    }
}
