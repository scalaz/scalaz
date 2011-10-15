package scalaz

trait Isomorphisms {

  /** Isomorphism for arrows of kind * -> * -> * */
  trait Iso[Arr[_, _], A, B] { self =>
    def to: Arr[A, B]
    def from: Arr[B, A]
    def swap = new Iso[Arr, B, A] {
      val to = self.from
      val from = self.to
    }
  }

  /** Isomorphism for arrows of kind (* -> *) -> (* -> *) -> * */
  trait Iso2[Arr[_[_], _[_]], F[_], G[_]] { self =>
    val to: Arr[F, G]
    val from: Arr[G, F]
    def swap = new Iso2[Arr, G, F] {
      val to = self.from
      val from = self.to
    }
  }

  /** Isomorphism for arrows of kind (* -> * -> *) -> (* -> * -> *) -> * */
  trait Iso3[Arr[_[_, _], _[_, _]], F[_, _], G[_, _]] { self =>
    def to: Arr[F, G]
    def from: Arr[G, F]
    def swap = new Iso3[Arr, G, F] {
      val to = self.from
      val from = self.to
    }
  }

  /** Set isomorphism */
  type IsoSet[A, B] = Iso[Function1, A, B]

  /** Natural isomorphism between functors */
  type IsoFunctor[F[_], G[_]] = Iso2[NaturalTransformation, F, G]

  type IsoBifunctor[F[_, _], G[_, _]] = Iso3[~~>, F, G]

  /** Alias for IsoSet */
  type <=>[A, B] = IsoSet[A, B]

  /** Alias for IsoFunctor */
  type <~>[F[_], G[_]] = IsoFunctor[F, G]

  /** Convenience template trait to implement `<~>` */
  trait IsoFunctorTemplate[F[_], G[_]] extends Iso2[NaturalTransformation, F, G] {
    final val to: NaturalTransformation[F, G] = new (F ~> G) {
      def apply[A](fa: F[A]): G[A] = to[A](fa)
    }
    final val from: NaturalTransformation[G, F] = new (G ~> F) {
      def apply[A](ga: G[A]): F[A] = from[A](ga)
    }

    def to[A](fa: F[A]): G[A]
    def from[A](ga: G[A]): F[A]
  }

  /** Alias for IsoBifunctor */
  type <~~>[F[_, _], G[_, _]] = IsoBifunctor[F, G]
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

trait IsomorphismFunctor[F[_], G[_]] extends Functor[F] {
  implicit def G: Functor[G]

  def iso: F <~> G

  override def map[A, B](fa: F[A])(f: A => B): F[B] = iso.from(G.map(iso.to(fa))(f))
}

trait IsomorphismPointed[F[_], G[_]] extends Pointed[F] with IsomorphismFunctor[F, G] {
  implicit def G: Pointed[G]

  def pure[A](a: => A): F[A] = iso.from(G.pure(a))
}

trait IsomorphismContravariant[F[_], G[_]] extends Contravariant[F] {
  implicit def G: Contravariant[G]

  def iso: F <~> G

  def contramap[A, B](r: F[A])(f: B => A): F[B] = iso.from(G.contramap(iso.to(r))(f))
}

trait IsomorphismCopointed[F[_], G[_]] extends Copointed[F] with IsomorphismContravariant[F, G] {
  implicit def G: Copointed[G]

  def copure[A](p: F[A]): A = G.copure(iso.to(p))
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

trait IsomorphismCojoin[F[_], G[_]] extends Cojoin[F] {
  implicit def G: Cojoin[G] with Functor[G]

  def iso: F <~> G

  def cojoin[A](a: F[A]): F[F[A]] = iso.from(G.map(G.cojoin(iso.to(a)))(iso.from.apply))
}

trait IsomorphismComonad[F[_], G[_]] extends Comonad[F] with IsomorphismCojoin[F, G] with IsomorphismCopointed[F, G] {
  implicit def G: Comonad[G] with Functor[G] with Copointed[G]
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

  def traverseImpl[H[_]: Applicative, A, B](fa: F[A])(f: (A) => H[B]): H[F[B]] = {
    Applicative[H].map(G.traverseImpl(iso.to(fa))(f))(iso.from.apply)
  }

  def foldR[A, B](fa: F[A], z: B)(f: (A) => (=> B) => B): B = G.foldR[A, B](iso.to(fa), z)(f)
}

