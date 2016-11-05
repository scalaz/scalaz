package scalaz

import Id._

/**
 * @see [[scalaz.Lens]]
 */
final case class IndexedStoreT[F[_], +I, A, B](run: (F[A => B], I)) {
  import StoreT._
  import BijectionT._

  def xmap[X1, X2](f: I => X1)(g: X2 => A)(implicit F: Functor[F]): IndexedStoreT[F, X1, X2, B] =
    indexedStoreT((F.map(set)(_ compose g), f(pos)))

  def bmap[X, Z >: I <: A](b: Bijection[Z, X])(implicit F: Functor[F]): StoreT[F, X, B] =
    xmap(b to _)(b from _)

  def imap[X](f: I => X): IndexedStoreT[F, X, A, B] =
    indexedStoreT((set, f(pos)))

  def contramap[X](g: X => A)(implicit F: Functor[F]) =
    indexedStoreT((F.map(set)(_ compose g), pos))

  def bimap[X, Y](f: I => X)(g: B => Y)(implicit F: Functor[F]): IndexedStoreT[F, X, A, Y] =
    indexedStoreT((F.map(set)(g compose _), f(pos)))

  def leftMap[X](f: I => X): IndexedStoreT[F, X, A, B] =
    imap(f)

  def put(a: A)(implicit F: Functor[F]): F[B] =
    F.map(run._1)(_(a))

  def puts(f: I => A)(implicit F: Functor[F]): F[B] =
    put(f(pos))

  def set: F[A => B] =
    run._1

  def pos: I =
    run._2

  def peek(a: A)(implicit F: Comonad[F]): B =
    F.copoint(set)(a)

  def peeks(f: I => A)(implicit F: Comonad[F]): B =
    F.copoint(set)(f(pos))

  def seek[J](j: J): IndexedStoreT[F, J, A, B] =
    indexedStoreT((set, j))

  def seeks[J](f: I => J): IndexedStoreT[F, J, A, B] =
    indexedStoreT((set, f(pos)))

  def experiment[G[_]](f: I => G[A])(implicit F: Comonad[F], G: Functor[G]): G[B] =
    G.map(f(pos))(F.copoint(set))

  def copoint(implicit F: Comonad[F], ev: I <:< A): B =
    F.copoint(run._1)(run._2)

  def map[C](f: B => C)(implicit ftr: Functor[F]): IndexedStoreT[F, I, A, C] =
    indexedStoreT(mapRunT(k => f compose k))

  def duplicate[J](implicit F: Comonad[F]): IndexedStoreT[F, I, J, IndexedStoreT[F, J, A, B]] =
    indexedStoreT((F.cobind(run._1)(ff => (a: J) => indexedStoreT((ff, a))), pos))

  def cobind[K, C](f: IndexedStoreT[F, K, A, B] => C)(implicit F: Cobind[F]): IndexedStoreT[F, I, K, C] =
    indexedStoreT((F.cobind(run._1)(ff => (a: K) => f(indexedStoreT((ff, a)))), pos))

  /** Two disjoint lenses can be paired */
  def product[J, C, D](that: IndexedStoreT[F, J, C, D])(implicit M: Bind[F]): IndexedStoreT[F, (I, J), (A, C), (B, D)] =
    IndexedStoreT(M.bind(set) { s => M.map(that.set)(t => { (ac: (A, C)) => (s(ac._1), t(ac._2))})}, (pos, that.pos))

  /** alias for `product` */
  def ***[J, C, D](that: IndexedStoreT[F, J, C, D])(implicit M: Bind[F]): IndexedStoreT[F, (I, J), (A, C), (B, D)] = product(that)

  private def mapRunT[C](f: (A => B) => C)(implicit F: Functor[F]): (F[C], I) =
    (F.map(run._1)(f), run._2)
}

object IndexedStoreT extends StoreTInstances with StoreTFunctions

trait IndexedStoreTFunctions {

  def indexedStoreT[F[_], I, A, B](r: (F[A => B], I)): IndexedStoreT[F, I, A, B] = IndexedStoreT(r)

  def indexedStore[I, A, B](i: I)(f: A => B): IndexedStore[I, A, B] =
    indexedStoreT[Id, I, A, B](f -> i)
}

trait StoreTFunctions extends IndexedStoreTFunctions {

  def storeT[F[_], A, B](r: (F[A => B], A)): StoreT[F, A, B] =
    indexedStoreT[F, A, A, B](r)

  def store[A, B](a: A)(f: A => B): Store[A, B] =
    storeT[Id, A, B](f -> a)
}
sealed abstract class IndexedStoreTInstances2 {
  implicit def indexedStoreTContravariant[F[_], I, B](implicit F0: Functor[F]): Contravariant[IndexedStoreT[F, I, ?, B]] =
    new IndexedStoreTContravariant[F, I, B] {
      implicit def F: Functor[F] = F0
    }
}
sealed abstract class IndexedStoreTInstances1 extends IndexedStoreTInstances2 {
  implicit def indexedStoreTFunctorLeft[F[_], A, B]: Functor[IndexedStoreT[F, ?, A, B]] =
    new IndexedStoreTFunctorLeft[F, A, B] {}
}
sealed abstract class IndexedStoreTInstances0 extends IndexedStoreTInstances1 {
  implicit def indexedStoreTBifunctor[F[_], A](implicit F0: Functor[F]): Bifunctor[IndexedStoreT[F, ?, A, ?]] =
    new IndexedStoreTBifunctor[F, A] {
      implicit def F: Functor[F] = F0
    }
}
sealed abstract class IndexedStoreTInstances extends IndexedStoreTInstances0 {
  implicit def indexedStoreTFunctorRight[F[_], I, A](implicit F0: Functor[F]): Functor[IndexedStoreT[F, I, A, ?]] =
    new IndexedStoreTFunctorRight[F, I, A] {
      implicit def F: Functor[F] = F0
    }
}
sealed abstract class StoreTInstances2 extends IndexedStoreTInstances {
  implicit def storeTCobind[F[_], A](implicit F0: Cobind[F]): Cobind[StoreT[F, A, ?]] =
    new StoreTCobind[F, A] {
      implicit def F: Cobind[F] = F0
    }
}
sealed abstract class StoreTInstances1 extends StoreTInstances2 {
  implicit def storeTComonad[F[_], A](implicit F0: Comonad[F]): Comonad[StoreT[F, A, ?]] =
    new StoreTComonad[F, A] {
      implicit def F: Comonad[F] = F0
    }
}
sealed abstract class StoreTInstances0 extends StoreTInstances1 {
  implicit def storeTComonadStore[F[_], A](implicit F0: Comonad[F]): ComonadStore[StoreT[F, A, ?], A] =
    new StoreTComonadStore[F, A] {
      implicit def F: Comonad[F] = F0
    }
}
abstract class StoreTInstances extends StoreTInstances0 {
  implicit def storeTCohoist[S]: Cohoist[λ[(ƒ[_], α) => StoreT[ƒ, S, α]]] =
    new StoreTCohoist[S] {}
}

private trait IndexedStoreTFunctorLeft[F[_], A0, B0] extends Functor[IndexedStoreT[F, ?, A0, B0]]{
  override def map[A, B](fa: IndexedStoreT[F, A, A0, B0])(f: A => B): IndexedStoreT[F, B, A0, B0] = fa imap f
}

private trait IndexedStoreTFunctorRight[F[_], I0, A0] extends Functor[IndexedStoreT[F, I0, A0, ?]]{
  implicit def F: Functor[F]
  override def map[A, B](fa: IndexedStoreT[F, I0, A0, A])(f: A => B): IndexedStoreT[F, I0, A0, B] = fa map f
}

private trait IndexedStoreTContravariant[F[_], I0, B0] extends Contravariant[IndexedStoreT[F, I0, ?, B0]] {
  implicit def F: Functor[F]
  override def contramap[A, B](fa: IndexedStoreT[F, I0, A, B0])(f: B => A): IndexedStoreT[F, I0, B, B0] = fa contramap f
}

private trait IndexedStoreTBifunctor[F[_], A0] extends Bifunctor[IndexedStoreT[F, ?, A0, ?]] {
  implicit def F: Functor[F]
  override def bimap[A, B, C, D](fab: IndexedStoreT[F, A, A0, B])(f: A => C, g: B => D): IndexedStoreT[F, C, A0, D] = (fab bimap f)(g)
}

private trait StoreTCobind[F[_], A0] extends Cobind[StoreT[F, A0, ?]] with IndexedStoreTFunctorRight[F, A0, A0] {
  implicit def F: Cobind[F]
  def cobind[A, B](fa: StoreT[F, A0, A])(f: (StoreT[F, A0, A]) => B) = fa cobind f
}

private trait StoreTComonad[F[_], A0] extends Comonad[StoreT[F, A0, ?]] with StoreTCobind[F, A0] {
  implicit def F: Comonad[F]
  override def cojoin[A](a: StoreT[F, A0, A]) = a.duplicate
  def copoint[A](p: StoreT[F, A0, A]) = p.copoint
}

private trait StoreTComonadStore[F[_], S] extends ComonadStore[StoreT[F, S, ?], S] with StoreTComonad[F, S] {
  def pos[A](w: StoreT[F, S, A]): S = w.pos
  def peek[A](s: S, w: StoreT[F, S, A]): A = w peek s
  override def peeks[A](s: S => S, w: StoreT[F, S, A]): A = w peeks s
  override def seek[A](s: S, w: StoreT[F, S, A]): StoreT[F, S, A] = w seek s
  override def seeks[A](s: S => S, w: StoreT[F, S, A]): StoreT[F, S, A] = w seeks s
  override def experiment[G[_], A](s: S => G[S], w: StoreT[F, S, A])(implicit FG: Functor[G]): G[A] = w experiment s
}

private trait StoreTCohoist[S] extends Cohoist[λ[(ƒ[_], α) => StoreT[ƒ, S, α]]] {
  def lower[G[_] : Cobind, A](a: StoreT[G, S, A]) =
    Cobind[G].map(a.run._1)((z: S => A) => z(a.run._2))

  def cohoist[M[_], N[_]: Comonad](f: M ~> N) =
    λ[StoreT[M, S, ?] ~> StoreT[N, S, ?]]{ c =>
      val (m, a) = c.run
      StoreT((f(m), a))
    }
}
