package scalaz

trait IsomorphismsLow1 {
  self: Isomorphisms =>

  /**Set isomorphism is commutative */
  implicit def isoCommutative[A, B](implicit i: A <=> B): B <=> A = i.flip

  /**Natural isomorphism is commutative */
  implicit def isoNaturalCommutative[F[_], G[_]](implicit i: F <~> G): G <~> F = i.flip
}

trait IsomorphismsLow0 extends IsomorphismsLow1 {
  self: Isomorphisms =>

  /**Set isomorphism is reflexive */
  implicit def isoRefl[A]: A <=> A = new (A <=> A) {
    def to: A => A = a => a
    def from: A => A = a => a
  }

  /**Natural isomorphism is reflexive */
  implicit def isoNaturalRefl[F[_]]: F <~> F = new IsoFunctorTemplate[F, F] {
    def to[A](fa: F[A]): F[A] = fa
    def from[A](fa: F[A]): F[A] = fa
  }
}

trait Isomorphisms extends IsomorphismsLow0{

  /**Isomorphism for arrows of kind * -> * -> * */
  trait Iso[Arr[_, _], A, B] {
    self =>
    def to: Arr[A, B]
    def from: Arr[B, A]
    def flip = new Iso[Arr, B, A] {
      val to = self.from
      val from = self.to
    }
  }

  /**Isomorphism for arrows of kind (* -> *) -> (* -> *) -> * */
  trait Iso2[Arr[_[_], _[_]], F[_], G[_]] {
    self =>
    def to: Arr[F, G]
    def from: Arr[G, F]
    def flip = new Iso2[Arr, G, F] {
      val to = self.from
      val from = self.to
    }
  }

  /**Isomorphism for arrows of kind (* -> * -> *) -> (* -> * -> *) -> * */
  trait Iso3[Arr[_[_, _], _[_, _]], F[_, _], G[_, _]] {
    self =>
    def to: Arr[F, G]
    def from: Arr[G, F]
    def flip = new Iso3[Arr, G, F] {
      val to = self.from
      val from = self.to
    }
  }

  /**Set isomorphism */
  type IsoSet[A, B] = Iso[Function1, A, B]

  /**Natural isomorphism between functors */
  type IsoFunctor[F[_], G[_]] = Iso2[NaturalTransformation, F, G]

  type IsoBiFunctor[F[_, _], G[_, _]] = Iso3[~~>, F, G]

  /**Alias for IsoSet */
  type <=>[A, B] = IsoSet[A, B]

  /**Alias for IsoFunctor */
  type <~>[F[_], G[_]] = IsoFunctor[F, G]

  /**Convenience template trait to implement `<~>` */
  trait IsoFunctorTemplate[F[_], G[_]] extends IsoFunctor[F, G] {
    final val to: NaturalTransformation[F, G] = new (F ~> G) {
      def apply[A](fa: F[A]): G[A] = to[A](fa)
    }
    final val from: NaturalTransformation[G, F] = new (G ~> F) {
      def apply[A](ga: G[A]): F[A] = from[A](ga)
    }

    def to[A](fa: F[A]): G[A]
    def from[A](ga: G[A]): F[A]
  }

  /**Alias for IsoBiFunctor */
  type <~~>[F[_, _], G[_, _]] = IsoBiFunctor[F, G]

  /**Convenience template trait to implement `<~~>` */
  trait IsoBiFunctorTemplate[F[_, _], G[_, _]] extends IsoBiFunctor[F, G] {
    final val to: BiNaturalTransformation[F, G] = new (F ~~> G) {
      def apply[A, B](fab: F[A, B]): G[A, B] = to[A, B](fab)
    }
    final val from: BiNaturalTransformation[G, F] = new (G ~~> F) {
      def apply[A, B](gab: G[A, B]): F[A, B] = from[A, B](gab)
    }

    def to[A, B](fa: F[A, B]): G[A, B]
    def from[A, B](ga: G[A, B]): F[A, B]
  }

}

object Isomorphism extends Isomorphisms

//
// Derive a type class instance through an Isomorphism
//

import Isomorphism._

trait IsomorphismSemigroup[F, G] extends Semigroup[F] {
  implicit def G: Semigroup[G]

  def iso: F <=> G

  def append(f1: F, f2: => F): F = iso.from(G.append(iso.to(f1), iso.to(f2)))
}

trait IsomorphismMonoid[F, G] extends Monoid[F] with IsomorphismSemigroup[F, G] {
  implicit def G: Monoid[G]

  def iso: F <=> G

  def zero: F = iso.from(G.zero)
}

trait IsomorphismEqual[F, G] extends Equal[F] {
  implicit def G: Equal[G]

  def iso: F <=> G

  def equal(a1: F, a2: F): Boolean = G.equal(iso.to(a1), iso.to(a2))
}

trait IsomorphismShow[F, G] extends Show[F] {
  implicit def G: Show[G]

  def iso: F <=> G

  def show(f: F): List[Char] = G.show(iso.to(f))
}

trait IsomorphismOrder[F, G] extends Order[F] {
  implicit def G: Order[G]

  def iso: F <=> G

  def order(x: F, y: F): Ordering = G.order(iso.to(x), iso.to(y))
}

trait IsomorphismEmpty[F[_], G[_]] extends Empty[F] {
  implicit def G: Empty[G]

  def iso: F <~> G

  def empty[A]: F[A] = iso.from(G.empty[A])
}

trait IsomorphismEach[F[_], G[_]] extends Each[F] {
  implicit def G: Each[G]

  def iso: F <~> G

  def each[A](fa: F[A])(f: (A) => Unit) = G.each(iso.to(fa))(f)
}

trait IsomorphismIndex[F[_], G[_]] extends Index[F] {
  implicit def G: Index[G]

  def iso: F <~> G

  def index[A](fa: F[A], n: Int): Option[A] = G.index(iso.to(fa), n)
}

trait IsomorphismFunctor[F[_], G[_]] extends Functor[F] {
  implicit def G: Functor[G]

  def iso: F <~> G

  override def map[A, B](fa: F[A])(f: A => B): F[B] = iso.from(G.map(iso.to(fa))(f))
}

trait IsomorphismPointed[F[_], G[_]] extends Pointed[F] with IsomorphismFunctor[F, G] {
  implicit def G: Pointed[G]

  def point[A](a: => A): F[A] = iso.from(G.point(a))
}

trait IsomorphismContravariant[F[_], G[_]] extends Contravariant[F] {
  implicit def G: Contravariant[G]

  def iso: F <~> G

  def contramap[A, B](r: F[A])(f: B => A): F[B] = iso.from(G.contramap(iso.to(r))(f))
}

trait IsomorphismCoPointed[F[_], G[_]] extends CoPointed[F] {
  implicit def G: CoPointed[G]

  def iso: F <~> G

  def copoint[A](p: F[A]): A = G.copoint(iso.to(p))
}

trait IsomorphismApply[F[_], G[_]] extends Apply[F] with IsomorphismFunctor[F, G] {
  implicit def G: Apply[G]

  override def ap[A, B](fa: F[A])(f: F[(A) => B]): F[B] = iso.from(G.ap(iso.to(fa))(iso.to(f)))
}

trait IsomorphismApplicative[F[_], G[_]] extends IsomorphismApply[F, G] with IsomorphismPointed[F, G] with Applicative[F] {
  implicit def G: Applicative[G]

  override def ap[A, B](fa: F[A])(f: F[(A) => B]): F[B] = iso.from(G.ap(iso.to(fa))(iso.to(f)))
}

trait IsomorphismBind[F[_], G[_]] extends IsomorphismApply[F, G] with Bind[F] {
  implicit def G: Bind[G]

  def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = iso.from(G.bind(iso.to(fa))(f.andThen(iso.to.apply)))
}

trait IsomorphismMonad[F[_], G[_]] extends IsomorphismApplicative[F, G] with IsomorphismBind[F, G] {
  implicit def G: Monad[G]
}

trait IsomorphismCoJoin[F[_], G[_]] extends CoJoin[F] {
  implicit def G: CoJoin[G] with Functor[G]

  def iso: F <~> G

  def cojoin[A](a: F[A]): F[F[A]] = iso.from(G.map(G.cojoin(iso.to(a)))(iso.from.apply))
}

trait IsomorphismCoMonad[F[_], G[_]] extends CoMonad[F] with IsomorphismCoJoin[F, G] with IsomorphismCoPointed[F, G] {
  implicit def G: CoMonad[G] with Functor[G] with CoPointed[G]
}

trait IsomorphismPlus[F[_], G[_]] extends Plus[F] with IsomorphismEmpty[F, G] with IsomorphismFunctor[F, G] {
  implicit def G: Plus[G]

  def iso: F <~> G

  def plus[A](a: F[A], b: => F[A]): F[A] = iso.from(G.plus(iso.to(a), iso.to(b)))
}

trait IsomorphismApplicativePlus[F[_], G[_]] extends ApplicativePlus[F] with IsomorphismPlus[F, G] with IsomorphismApplicative[F, G] {
  implicit def G: ApplicativePlus[G]
}

trait IsomorphismMonadPlus[F[_], G[_]] extends MonadPlus[F] with IsomorphismPlus[F, G] with IsomorphismMonad[F, G] {
  implicit def G: MonadPlus[G]
}

trait IsomorphismTraverse[F[_], G[_]] extends Traverse[F] with IsomorphismFunctor[F, G] {
  implicit def G: Traverse[G]

  def traverseImpl[H[_] : Applicative, A, B](fa: F[A])(f: (A) => H[B]): H[F[B]] = {
    Applicative[H].map(G.traverseImpl(iso.to(fa))(f))(iso.from.apply)
  }

  def foldR[A, B](fa: F[A], z: B)(f: (A) => (=> B) => B): B = G.foldR[A, B](iso.to(fa), z)(f)
}

trait IsomorphismBiFunctor[F[_, _], G[_, _]] extends BiFunctor[F] {
  def iso: F <~~> G

  implicit def G: BiFunctor[G]

  override def bimap[A, B, C, D](fab: F[A, B])(f: (A) => C, g: (B) => D): F[C, D] =
    iso.from(G.bimap(iso.to(fab))(f, g))
}

trait IsomorphismBiTraverse[F[_, _], G[_, _]] extends BiTraverse[F] with IsomorphismBiFunctor[F, G] {
  implicit def G: BiTraverse[G]

  def bitraverse[H[_]: Applicative, A, B, C, D](fab: F[A, B])(f: (A) => H[C], g: (B) => H[D]): H[F[C, D]] =
    Applicative[H].map(G.bitraverse(iso.to(fab))(f, g))(iso.from.apply)
}

