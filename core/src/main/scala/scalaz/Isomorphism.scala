package scalaz

sealed abstract class IsomorphismsLow1 {
  self: Isomorphisms =>

  /**Set isomorphism is commutative */
  implicit def isoCommutative[A, B](implicit i: A <=> B): B <=> A = i.flip

  /**Natural isomorphism is commutative */
  implicit def isoNaturalCommutative[F[_], G[_]](implicit i: F <~> G): G <~> F = i.flip
}

sealed abstract class IsomorphismsLow0 extends IsomorphismsLow1 {
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

sealed abstract class Isomorphisms extends IsomorphismsLow0{

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
        def from = GF(self.from)
        def to   = FG(self.to)
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
    ): F[A, ?] <~> G[A, ?] = {
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
    ): F[?, A] <~> G[?, A] = {
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

  /**Alias for IsoBifunctor */
  type <~~>[F[_, _], G[_, _]] = IsoBifunctor[F, G]

  /**Convenience template trait to implement `<~~>` */
  trait IsoBifunctorTemplate[F[_, _], G[_, _]] extends IsoBifunctor[F, G] {
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

  override def show(f: F): Cord = G.show(iso.to(f))
}

trait IsomorphismOrder[F, G] extends Order[F] {
  implicit def G: Order[G]

  def iso: F <=> G

  override def order(x: F, y: F): Ordering = G.order(iso.to(x), iso.to(y))
}

trait IsomorphismEnum[F, G] extends Enum[F] {
  implicit def G: Enum[G]

  def iso: F <=> G

  def succ(a: F): F =
    iso.from(G.succ(iso.to(a)))

  def pred(a: F): F =
    iso.from(G.pred(iso.to(a)))
}

trait IsomorphismFunctor[F[_], G[_]] extends Functor[F] {
  implicit def G: Functor[G]

  def iso: F <~> G

  override def map[A, B](fa: F[A])(f: A => B): F[B] = iso.from(G.map(iso.to(fa))(f))
}

trait IsomorphismContravariant[F[_], G[_]] extends Contravariant[F] {
  implicit def G: Contravariant[G]

  def iso: F <~> G

  override def contramap[A, B](r: F[A])(f: B => A): F[B] = iso.from(G.contramap(iso.to(r))(f))
}

trait IsomorphismInvariantFunctor[F[_], G[_]] extends InvariantFunctor[F]  {
  implicit def G: InvariantFunctor[G]

  def iso: F <~> G

  override def xmap[A, B](ma: F[A], f: A => B, g: B => A): F[B] =
    iso.from(G.xmap(iso.to(ma), f, g))
}

trait IsomorphismDivide[F[_], G[_]] extends Divide[F] with IsomorphismContravariant[F, G] {
  implicit def G: Divide[G]

  override def divide[A, B, C](fa: F[A], fb: F[B])(f: C => (A, B)): F[C] =
    iso.from(G.divide(iso.to(fa), iso.to(fb))(f))
}

trait IsomorphismDivisible[F[_], G[_]] extends Divisible[F] with IsomorphismDivide[F, G] {
  implicit def G: Divisible[G]

  override def conquer[A]: F[A] =
    iso.from(G.conquer)
}


trait IsomorphismApply[F[_], G[_]] extends Apply[F] with IsomorphismFunctor[F, G] {
  implicit def G: Apply[G]

  override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = iso.from(G.ap(iso.to(fa))(iso.to(f)))
}

trait IsomorphismAlign[F[_], G[_]] extends Align[F] with IsomorphismFunctor[F, G] {
  implicit def G: Align[G]

  import \&/._

  override def alignWith[A, B, C](f: A \&/ B => C): (F[A], F[B]) => F[C] = {
    case (fa, fb) => iso.from(G.alignWith(f)(iso.to(fa), iso.to(fb)))
  }
}

trait IsomorphismApplicative[F[_], G[_]] extends Applicative[F] with IsomorphismApply[F, G] {
  implicit def G: Applicative[G]

  def point[A](a: => A): F[A] = iso.from(G.point(a))

  override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = iso.from(G.ap(iso.to(fa))(iso.to(f)))
}

trait IsomorphismBind[F[_], G[_]] extends Bind[F] with IsomorphismApply[F, G] {
  implicit def G: Bind[G]

  def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = iso.from(G.bind(iso.to(fa))(f.andThen(iso.to.apply)))
}

trait IsomorphismBindRec[F[_], G[_]] extends BindRec[F] with IsomorphismBind[F, G] {
  implicit def G: BindRec[G]

  override def tailrecM[A, B](f: A => F[A \/ B])(a: A): F[B] = iso.from(G.tailrecM(f andThen iso.unlift[A \/ B].to)(a))
}

trait IsomorphismMonad[F[_], G[_]] extends Monad[F] with IsomorphismApplicative[F, G] with IsomorphismBind[F, G] {
  implicit def G: Monad[G]
}

trait IsomorphismMonadReader[F[_], G[_], E] extends MonadReader[F, E] with IsomorphismMonad[F, G] {
  implicit def G: MonadReader[G, E]

  override def ask: F[E] = iso.from(G.ask)

  override def local[A](f: (E) => E)(fa: F[A]): F[A] =
    iso.from(G.local(f)(iso.to(fa)))
}

trait IsomorphismMonadState[F[_], G[_], S] extends MonadState[F, S] with IsomorphismMonad[F, G] {
  implicit def G: MonadState[G, S]

  override def init: F[S] = iso.from(G.init)

  override def get: F[S] = iso.from(G.get)

  override def put(s: S): F[Unit] = iso.from(G.put(s))
}


trait IsomorphismMonadTell[F[_], G[_], S] extends MonadTell[F, S] with IsomorphismMonad[F, G] {
  implicit def G: MonadTell[G, S]

  override def writer[A](w: S, v: A): F[A] = iso.from(G.writer(w, v))
}

trait IsomorphismMonadError[F[_], G[_], S] extends MonadError[F, S] with IsomorphismMonad[F, G] {
  implicit def G: MonadError[G, S]

  override def raiseError[A](e: S): F[A] =
    iso.from(G.raiseError(e))

  override def handleError[A](fa: F[A])(f: S => F[A]): F[A] =
    iso.from(G.handleError(iso.to(fa))(s => iso.to(f(s))))
}

trait IsomorphismNondeterminism[F[_], G[_]] extends Nondeterminism[F] with IsomorphismMonad[F, G] {
  implicit def G: Nondeterminism[G]

  override def chooseAny[A](head: F[A], tail: Seq[F[A]]): F[(A, Seq[F[A]])] =
    iso.from(G.map(G.chooseAny(iso.to(head), tail.map(iso.to))){case (a, b) => (a, b.map(iso.from))})
}

trait IsomorphismCobind[F[_], G[_]] extends Cobind[F] with IsomorphismFunctor[F, G] {
  implicit def G: Cobind[G]

  override def cobind[A, B](fa: F[A])(f: F[A] => B): F[B] = iso.from(G.cobind(iso.to(fa))(f.compose(iso.from.apply)))

  override def cojoin[A](a: F[A]): F[F[A]] = iso.from(G.map(G.cojoin(iso.to(a)))(iso.from.apply))
}

trait IsomorphismComonad[F[_], G[_]] extends Comonad[F] with IsomorphismCobind[F, G] {
  implicit def G: Comonad[G]

  override def copoint[A](p: F[A]): A = G.copoint(iso.to(p))
}


trait IsomorphismComonadStore[F[_], G[_], S] extends ComonadStore[F, S] with IsomorphismComonad[F, G] {
  implicit def G: ComonadStore[G, S]

  override def pos[A](w: F[A]): S
    = G.pos(iso.to(w))

  override def peek[A](s: S, w: F[A]): A
    = G.peek(s, iso.to(w))

}

trait IsomorphismPlus[F[_], G[_]] extends Plus[F] {
  implicit def G: Plus[G]

  def iso: F <~> G

  def plus[A](a: F[A], b: => F[A]): F[A] = iso.from(G.plus(iso.to(a), iso.to(b)))
}

trait IsomorphismEmpty[F[_], G[_]] extends PlusEmpty[F] with IsomorphismPlus[F, G] {
  implicit def G: PlusEmpty[G]

  def empty[A]: F[A] = iso.from(G.empty[A])
}

trait IsomorphismIsEmpty[F[_], G[_]] extends IsEmpty[F] with IsomorphismEmpty[F, G] {
  implicit def G: IsEmpty[G]

  def isEmpty[A](fa: F[A]): Boolean =
    G.isEmpty(iso.to(fa))
}

trait IsomorphismApplicativePlus[F[_], G[_]] extends ApplicativePlus[F] with IsomorphismEmpty[F, G] with IsomorphismApplicative[F, G] {
  implicit def G: ApplicativePlus[G]
}

trait IsomorphismMonadPlus[F[_], G[_]] extends MonadPlus[F] with IsomorphismEmpty[F, G] with IsomorphismMonad[F, G] {
  implicit def G: MonadPlus[G]
}

trait IsomorphismFoldable[F[_], G[_]] extends Foldable[F] {
  implicit def G: Foldable[G]

  protected[this] def naturalTrans: F ~> G

  override def foldMap[A, B](fa: F[A])(f: A => B)(implicit F: Monoid[B]) = G.foldMap(naturalTrans(fa))(f)

  override def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B) = G.foldLeft(naturalTrans(fa), z)(f)

  override def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B = G.foldRight[A, B](naturalTrans(fa), z)(f)
}

trait IsomorphismTraverse[F[_], G[_]] extends Traverse[F] with IsomorphismFoldable[F, G] with IsomorphismFunctor[F, G] {
  implicit def G: Traverse[G]

  protected[this] override final def naturalTrans: F ~> G = iso.to

  override def traverseImpl[H[_] : Applicative, A, B](fa: F[A])(f: A => H[B]): H[F[B]] =
    Applicative[H].map(G.traverseImpl(iso.to(fa))(f))(iso.from.apply)
}

trait IsomorphismFoldable1[F[_], G[_]] extends Foldable1[F] with IsomorphismFoldable[F, G] {
  implicit def G: Foldable1[G]

  override final def foldMap1[A, B: Semigroup](fa: F[A])(f: A => B): B = G.foldMap1(naturalTrans(fa))(f)

  override final def foldMapLeft1[A, B](fa: F[A])(z: A => B)(f: (B, A) => B): B = G.foldMapLeft1(naturalTrans(fa))(z)(f)

  override final def foldMapRight1[A, B](fa: F[A])(z: A => B)(f: (A, => B) => B): B = G.foldMapRight1(naturalTrans(fa))(z)(f)
}

trait IsomorphismTraverse1[F[_], G[_]] extends Traverse1[F] with IsomorphismTraverse[F, G] with IsomorphismFoldable1[F, G] {
  implicit def G: Traverse1[G]

  override def traverse1Impl[H[_]: Apply, A, B](fa: F[A])(f: A => H[B]): H[F[B]] =
    Apply[H].map(G.traverse1Impl(iso.to(fa))(f))(iso.from.apply)
}

trait IsomorphismOptional[F[_], G[_]] extends Optional[F] {
  implicit def G: Optional[G]

  def iso: F <~> G

  override def pextract[B, A](fa: F[A]): F[B] \/ A =
    G.pextract(iso.to(fa)).leftMap(iso.from)
}

trait IsomorphismCatchable[F[_], G[_]] extends Catchable[F] {
  implicit def G: Catchable[G]

  def iso: F <~> G

  override def attempt[A](f: F[A]): F[Throwable \/ A] =
    iso.from(G.attempt(iso.to(f)))

  override def fail[A](err: Throwable): F[A] =
    iso.from(G.fail(err))
}


trait IsomorphismCozip[F[_], G[_]] extends Cozip[F] {
  implicit def G: Cozip[G]

  def iso: F <~> G

  def cozip[A, B](x: F[A \/ B]): (F[A] \/ F[B]) =
    G.cozip(iso.to(x)).bimap(iso.from.apply _, iso.from.apply _)
}


trait IsomorphismZip[F[_], G[_]] extends Zip[F] {
  implicit def G: Zip[G]

  def iso: F <~> G

  def zip[A, B](a: => F[A], b: => F[B]): F[(A, B)] =
    iso.from(G.zip(iso.to(a), iso.to(b)))
}

trait IsomorphismUnzip[F[_], G[_]] extends Unzip[F] {
  implicit def G: Unzip[G]

  def iso: F <~> G

  def unzip[A, B](a: F[(A, B)]): (F[A], F[B]) =
    G.unzip(iso.to(a)) match {
      case (f, s) => (iso.from(f), iso.from(s))
    }
}

trait IsomorphismAssociative[F[_, _], G[_, _]] extends Associative[F] {
  implicit def G: Associative[G] with Bifunctor[G] // TODO: is this needed? (I think so)

  def iso: F <~~> G

  override def reassociateLeft[A, B, C](f: F[A, F[B, C]]): F[F[A, B], C] =
    iso.from(G.leftMap(G.reassociateLeft(G.rightMap(iso.to(f))(iso.to.apply _)))(iso.from.apply _))

  override def reassociateRight[A, B, C](f: F[F[A, B], C]): F[A, F[B, C]] =
    iso.from(G.rightMap(G.reassociateRight(G.leftMap(iso.to(f))(iso.to.apply _)))(iso.from.apply _))
}


trait IsomorphismBifunctor[F[_, _], G[_, _]] extends Bifunctor[F] {
  implicit def G: Bifunctor[G]

  def iso: F <~~> G

  override def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] =
    iso.from(G.bimap(iso.to(fab))(f, g))
}


trait IsomorphismProfunctor[F[_, _], G[_, _]] extends Profunctor[F] {
  implicit def G: Profunctor[G]

  def iso: F <~~> G

  override def mapfst[A, B, C](fab: F[A, B])(f: C => A): F[C, B] =
    iso.from(G.mapfst(iso.to(fab))(f))

  override def mapsnd[A, B, C](fab: F[A, B])(f: B => C): F[A, C] =
    iso.from(G.mapsnd(iso.to(fab))(f))

}

trait IsomorphismProChoice[F[_, _], G[_, _]] extends ProChoice[F] with IsomorphismProfunctor[F, G] {
  implicit def G: ProChoice[G]

  override def left[A, B, C](fa: F[A, B]): F[(A \/ C), (B \/ C)] =
    iso.from(G.left(iso.to(fa)))

  override def right[A, B, C](fa: F[A, B]): F[(C \/ A), (C \/ B)] =
    iso.from(G.right(iso.to(fa)))
}

trait IsomorphismStrong[F[_, _], G[_, _]] extends Strong[F] with IsomorphismProfunctor[F, G] {
  implicit def G: Strong[G]

  override def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)] =
    iso.from(G.first(iso.to(fa)))

  override def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)] =
    iso.from(G.second(iso.to(fa)))
}

trait IsomorphismCompose[F[_, _], G[_, _]] extends Compose[F] {
  implicit def G: Compose[G]

  def iso: F <~~> G

  override def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] =
    iso.from(G.compose(iso.to(f), iso.to(g)))
}

trait IsomorphismCategory[F[_, _], G[_, _]] extends Category[F] with IsomorphismCompose[F, G] {
  implicit def G: Category[G]

  override def id[A]: F[A, A] =
    iso.from(G.id)
}


trait IsomorphismChoice[F[_, _], G[_, _]] extends Choice[F] with IsomorphismCategory[F, G] {
  implicit def G: Choice[G]

  override def choice[A, B, C](f: => F[A, C], g: => F[B, C]): F[(A \/ B), C] =
    iso.from(G.choice(iso.to(f), iso.to(g)))
}

trait IsomorphismSplit[F[_, _], G[_, _]] extends Split[F] with IsomorphismCompose[F, G] {
  implicit def G: Split[G]

  override def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A,  C), (B, D)] =
    iso.from(G.split(iso.to(f), iso.to(g)))
}

trait IsomorphismArrow[F[_, _], G[_, _]] extends Arrow[F] with IsomorphismSplit[F, G] with IsomorphismStrong[F, G] with IsomorphismCategory[F, G] {
  implicit def G: Arrow[G]

  override def arr[A, B](f: A => B): F[A, B] =
    iso.from(G.arr(f))
}


trait IsomorphismBifoldable[F[_, _], G[_, _]] extends Bifoldable[F] {
  protected[this] def biNaturalTrans: F ~~> G

  implicit def G: Bifoldable[G]

  override final def bifoldMap[A, B, M: Monoid](fab: F[A, B])(f: A => M)(g: B => M): M =
    G.bifoldMap(biNaturalTrans(fab))(f)(g)

  override final def bifoldRight[A, B, C](fab: F[A, B], z: => C)(f: (A, => C) => C)(g: (B, => C) => C): C =
    G.bifoldRight(biNaturalTrans(fab), z)(f)(g)

  override final def bifoldLeft[A, B, C](fa: F[A, B], z: C)(f: (C, A) => C)(g: (C, B) => C): C =
    G.bifoldLeft(biNaturalTrans(fa), z)(f)(g)
}

trait IsomorphismBitraverse[F[_, _], G[_, _]] extends Bitraverse[F] with IsomorphismBifunctor[F, G] with IsomorphismBifoldable[F, G] {
  override final protected[this] def biNaturalTrans: F ~~> G = iso.to

  implicit def G: Bitraverse[G]

  def bitraverseImpl[H[_]: Applicative, A, B, C, D](fab: F[A, B])(f: A => H[C], g: B => H[D]): H[F[C, D]] =
    Applicative[H].map(G.bitraverseImpl(iso.to(fab))(f, g))(iso.from.apply)
}
