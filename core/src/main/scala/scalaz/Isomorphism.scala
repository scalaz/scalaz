package scalaz

sealed abstract class Isomorphisms {

  /**Isomorphism for arrows of kind * -> * -> * */
  trait Iso[Arr[_, _], A, B] {
    self =>
    def to: Arr[A, B]
    def from: Arr[B, A]
    def flip: Iso[Arr, B, A] = new Iso[Arr, B, A] {
      val to = self.from
      val from = self.to
      override def flip = self
    }

    def %~(f: Arr[B, B])(implicit C: Compose[Arr]): Arr[A, A] =
      C.compose(from, C.compose(f, to))
  }

  /**Isomorphism for arrows of kind (* -> *) -> (* -> *) -> * */
  trait Iso2[Arr[_[_], _[_]], F[_], G[_]] {
    self =>
    def to: Arr[F, G]
    def from: Arr[G, F]
    def flip: Iso2[Arr, G, F] = new Iso2[Arr, G, F] {
      val to = self.from
      val from = self.to
      override def flip = self
    }

    import Liskov._

    def unlift[A](implicit FG: Arr[F, G] <~< (F ~> G), GF: Arr[G, F] <~< (G ~> F)): F[A] <=> G[A] =
      new (F[A] <=> G[A]){
        def from = GF(self.from).apply
        def to   = FG(self.to).apply
      }

    def %~(f: G ~> G)(implicit FG: Arr[F, G] <~< (F ~> G), GF: Arr[G, F] <~< (G ~> F)): F ~> F =
      new (F ~> F) {
        def apply[A](a: F[A]): F[A] = GF(self.from)(f(FG(self.to)(a)))
      }
  }

  /**Isomorphism for arrows of kind (* -> * -> *) -> (* -> * -> *) -> * */
  trait Iso3[Arr[_[_, _], _[_, _]], F[_, _], G[_, _]] {
    self =>
    def to: Arr[F, G]
    def from: Arr[G, F]
    def flip: Iso3[Arr, G, F] = new Iso3[Arr, G, F] {
      val to = self.from
      val from = self.to
      override def flip = self
    }

    import Liskov._

    def unlift[A, B](implicit
      FG: Arr[F, G] <~< (F ~~> G),
      GF: Arr[G, F] <~< (G ~~> F)
    ): F[A, B] <=> G[A, B] =
      new (F[A, B] <=> G[A, B]){
        def from = GF(self.from).apply _
        def to   = FG(self.to).apply _
      }

    def unlift1[A](implicit
      FG: Arr[F, G] <~< (F ~~> G),
      GF: Arr[G, F] <~< (G ~~> F)
    ): F[A, *] <~> G[A, *] = {
      type FA[α] = F[A, α]
      type GA[α] = G[A, α]
      new IsoFunctorTemplate[FA, GA]{
        def from[X](ga: GA[X]) = GF(self.from)(ga)
        def to[X](fa: FA[X]) = FG(self.to)(fa)
      }
    }

    def unlift2[A](implicit
      FG: Arr[F, G] <~< (F ~~> G),
      GF: Arr[G, F] <~< (G ~~> F)
    ): F[*, A] <~> G[*, A] = {
      type FA[α] = F[α, A]
      type GA[α] = G[α, A]
      new IsoFunctorTemplate[FA, GA]{
        def from[X](ga: GA[X]) = GF(self.from)(ga)
        def to[X](fa: FA[X]) = FG(self.to)(fa)
      }
    }

    def %~(f: G ~~> G)(implicit FG: Arr[F, G] <~< (F ~~> G), GF: Arr[G, F] <~< (G ~~> F)): F ~~> F =
      new (F ~~> F) {
        def apply[A, B](a: F[A, B]): F[A, B] = GF(self.from)(f(FG(self.to)(a)))
      }
  }

  /**Set isomorphism */
  type IsoSet[A, B] = Iso[Function1, A, B]

  /**Natural isomorphism between functors */
  type IsoFunctor[F[_], G[_]] = Iso2[NaturalTransformation, F, G]

  type IsoBifunctor[F[_, _], G[_, _]] = Iso3[~~>, F, G]

  /**Alias for IsoSet */
  type <=>[A, B] = IsoSet[A, B]

  object IsoSet {
    /**Convenience constructor to implement `A <=> B` from `A => B` and `B => A` */
    def apply[A, B](to: A => B, from: B => A): A <=> B = {
      val _to   = to
      val _from = from
      new Iso[Function1, A, B] {
        override val to   = _to
        override val from = _from
      }
    }
  }

  /**Alias for IsoFunctor */
  type <~>[F[_], G[_]] = IsoFunctor[F, G]

  /**Convenience template trait to implement `<~>` */
  trait IsoFunctorTemplate[F[_], G[_]] extends IsoFunctor[F, G] with IsoFunctorTemplate0[F, G] {
    override final val to: NaturalTransformation[F, G] = new (F ~> G) {
      def apply[A](fa: F[A]): G[A] = to_[A](fa)
    }
    override final val from: NaturalTransformation[G, F] = new (G ~> F) {
      def apply[A](ga: G[A]): F[A] = from_[A](ga)
    }
  }

  object IsoFunctor {
    /**Convenience constructor to implement `F <~> G` from F ~> G and G ~> F */
    def apply[F[_], G[_]](to: F ~> G, from: G ~> F): F <~> G = {
      val _to   = to
      val _from = from
      new (F <~> G) {
        override val to = _to
        override val from = _from
      }
    }
  }

  /**Alias for IsoBifunctor */
  type <~~>[F[_, _], G[_, _]] = IsoBifunctor[F, G]

  /**Convenience template trait to implement `<~~>` */
  trait IsoBifunctorTemplate[F[_, _], G[_, _]] extends IsoBifunctor[F, G] with IsoBifunctorTemplate0[F, G] {
    final val to: BiNaturalTransformation[F, G] = new (F ~~> G) {
      def apply[A, B](fab: F[A, B]): G[A, B] = to_[A, B](fab)
    }
    final val from: BiNaturalTransformation[G, F] = new (G ~~> F) {
      def apply[A, B](gab: G[A, B]): F[A, B] = from_[A, B](gab)
    }
  }

  /**Set isomorphism is commutative */
  def commutative[A, B](i: A <=> B): B <=> A = i.flip

  /**Set isomorphism is reflexive */
  def refl[A]: A <=> A = new (A <=> A) {
    def to: A => A = a => a
    def from: A => A = a => a
  }

  /**Natural isomorphism is reflexive */
  def naturalRefl[F[_]]: F <~> F = new IsoFunctorTemplate[F, F] {
    def to[A](fa: F[A]): F[A] = fa
    def from[A](fa: F[A]): F[A] = fa
  }

  /**Natural isomorphism is commutative */
  def naturalCommutative[F[_], G[_]](i: F <~> G): G <~> F = i.flip

}

object Isomorphism extends Isomorphisms

import Isomorphism._

trait IsomorphismAssociative[F[_, _], G[_, _]] extends Associative[F] {
  implicit def G: Associative[G] with Bifunctor[G] // TODO: is this needed? (I think so)

  def iso: F <~~> G

  override def reassociateLeft[A, B, C](f: F[A, F[B, C]]): F[F[A, B], C] =
    iso.from(G.leftMap(G.reassociateLeft(G.rightMap(iso.to(f))(iso.to.apply _)))(iso.from.apply _))

  override def reassociateRight[A, B, C](f: F[F[A, B], C]): F[A, F[B, C]] =
    iso.from(G.rightMap(G.reassociateRight(G.leftMap(iso.to(f))(iso.to.apply _)))(iso.from.apply _))
}

sealed trait IsoFunctorTemplate0[F[_], G[_]] {
  def to[A](fa: F[A]): G[A]
  def from[A](ga: G[A]): F[A]

  def to_[A](fa: F[A]): G[A] = to[A](fa)
  def from_[A](ga: G[A]): F[A] = from[A](ga)
}

sealed trait IsoBifunctorTemplate0[F[_, _], G[_, _]] {
  def to[A, B](fa: F[A, B]): G[A, B]
  def from[A, B](ga: G[A, B]): F[A, B]

  def to_[A, B](fa: F[A, B]): G[A, B] = to[A, B](fa)
  def from_[A, B](ga: G[A, B]): F[A, B] = from[A, B](ga)
}
