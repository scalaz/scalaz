package scalaz
package typelevel

import syntax.typelevel.hlist._

/**
 * A type class abstracting over the `product` and `compose` operations of
 * type classes over types of kind `* -> *`.
 *
 * It serves as a replacement for the various `Product*` classes in
 * [[scalaz]].
 *
 * Multiplication of instances is done via wrappers which are unpacked
 * automatically if the implicits of
 * [[scalaz.typelevel.syntax.TypeClasses]] are imported.
 *
 * @see [[scalaz.typelevel.syntax.TypeClasses]]
 * @see [[scalaz.typelevel.TCList]]
 */
trait KTypeClass[C[_[_]]] {

  import KTypeClass._

  /**
   * Given a type class instance for `F`, and a type class instance for a
   * product, produce a type class instance for the product prepended with `F`.
   *
   * @note Users of this class should start with `emptyProduct` and use the
   * functions on [[scalaz.typelevel.KTypeClass.WrappedProduct]], which greatly
   * improves type inference.
   */
  def product[F[_], T <: TCList](FHead: C[F], FTail: C[T#Product]): C[TCCons[F, T]#Product]

  /**
   * The empty product.
   *
   * @return an instance of [[scalaz.typelevel.KTypeClass.WrappedProduct]]
   * representing an empty product
   */
  final def emptyProduct = new WrappedProduct[C, TCNil](_emptyProduct, this)

  /** The unwrapped product containing one element. */
  final def product1[F[_]](implicit F: C[F]): C[TCCons[F, TCNil]#Product] =
    (emptyProduct prodLeft F).instance

  /**The unwrapped product containing two elements. */
  final def product2[F[_], G[_]](implicit F: C[F], G: C[G]): C[TCCons[F, TCCons[G, TCNil]]#Product] =
    (emptyProduct prodLeft G prodLeft F).instance

  /**The unwrapped product containing three elements. */
  final def product3[F[_], G[_], H[_]](implicit F: C[F], G: C[G], H: C[H]): C[TCCons[F, TCCons[G, TCCons[H, TCNil]]]#Product] =
    (emptyProduct prodLeft H prodLeft G prodLeft F).instance


  /**
   * Given a type class instance for `F`, and a type class instance for a
   * composition, produce a type class instance for the composition wrapped by
   * `F`.
   *
   * @note Users of this class should start with `idCompose` and use the
   * functions on [[scalaz.typelevel.KTypeClass.WrappedComposition]], which greatly
   * improves type inference.
   */
  def compose[F[_], T <: TCList](FOuter: C[F], FInner: C[T#Composed]): C[TCCons[F, T]#Composed]

  /**
   * The empty composition.
   *
   * @return an instance of [[scalaz.typelevel.TypeClass.WrappedComposition]]
   * representing an empty composition
   */
  final def idCompose = new WrappedComposition[C, TCNil](_idCompose, this)



  // Implementation

  // implement this function to return a type class performing identity operations on `HNil`
  protected def _emptyProduct: C[TCNil#Product]

  // implement this function to return the type class instance for `Id`
  protected def _idCompose: C[TCNil#Composed]

}

object KTypeClass {

  @inline def apply[C[_[_]]](implicit C: KTypeClass[C]) = C

  /** Wrapping the computation of a product. */
  class WrappedProduct[C[_[_]], T <: TCList](val instance: C[T#Product], typeClass: KTypeClass[C]) {

    def *:[F[_]](F: C[F]) = new WrappedProduct[C, TCCons[F, T]](typeClass.product(F, instance), typeClass)

    def prodLeft[F[_]](F: C[F]) = F *: this

  }

  /** Wrapping the computation of a composition. */
  class WrappedComposition[C[_[_]], T <: TCList](val instance: C[T#Composed], typeClass: KTypeClass[C]) {

    def <<:[F[_]](F: C[F]) = new WrappedComposition[C, TCCons[F, T]](typeClass.compose[F, T](F, instance), typeClass)

    def :>>[F[_]](F: C[F]) = F <<: this

    def composeInto[F[_]](F: C[F]) = F <<: this

  }

  // The following classes are in place mostly to avoid repetition of methods
  // of type classes which are inherited from some other ones.

  // Products

  private[scalaz] trait Product[+C[_[_]], F[_], T <: TCList] {
    def FHead: C[F]
    def FTail: C[T#Product]

    type λ[α] = F[α] :: T#Product[α]
  }

  private[scalaz] trait ProductFunctor[F[_], T <: TCList]
    extends Functor[TCCons[F, T]#Product]
    with Product[Functor, F, T] {

    def map[A, B](fa: λ[A])(f: A => B) =
      FHead.map(fa.head)(f) :: FTail.map(fa.tail)(f)

  }

  private[scalaz] trait ProductPointed[F[_], T <: TCList]
    extends ProductFunctor[F, T]
    with Pointed[TCCons[F, T]#Product]
    with Product[Pointed, F, T] {

    def point[A](a: => A) =
      FHead.point(a) :: FTail.point(a)

  }

  private[scalaz] trait ProductApply[F[_], T <: TCList]
    extends ProductFunctor[F, T]
    with Apply[TCCons[F, T]#Product]
    with Product[Apply, F, T] {

    def ap[A, B](fa: => λ[A])(f: => λ[A => B]) =
      FHead.ap(fa.head)(f.head) :: FTail.ap(fa.tail)(f.tail)

  }

  private[scalaz] trait ProductApplicative[F[_], T <: TCList]
    extends ProductPointed[F, T]
    with ProductApply[F, T]
    with Applicative[TCCons[F, T]#Product]
    with Product[Applicative, F, T]

  private[scalaz] trait ProductPlus[F[_], T <: TCList]
    extends Plus[TCCons[F, T]#Product]
    with Product[Plus, F, T] {

    def plus[A](a: λ[A], b: => λ[A]) =
      FHead.plus(a.head, b.head) :: FTail.plus(a.tail, b.tail)

  }

  private[scalaz] trait ProductPlusEmpty[F[_], T <: TCList]
    extends ProductPlus[F, T]
    with PlusEmpty[TCCons[F, T]#Product]
    with Product[PlusEmpty, F, T] {

    def empty[A] = FHead.empty[A] :: FTail.empty[A]

  }

  private[scalaz] trait ProductApplicativePlus[F[_], T <: TCList]
    extends ProductPlusEmpty[F, T]
    with ProductApplicative[F, T]
    with ApplicativePlus[TCCons[F, T]#Product]
    with Product[ApplicativePlus, F, T]

  private[scalaz] trait ProductFoldable[F[_], T <: TCList]
    extends Foldable[TCCons[F, T]#Product]
    with Product[Foldable, F, T] {

    def foldMap[A, B](fa: λ[A])(f: A => B)(implicit M: Monoid[B]) =
      M.append(FHead.foldMap(fa.head)(f), FTail.foldMap(fa.tail)(f))

    def foldRight[A, B](fa: λ[A], z: => B)(f: (A, => B) => B) =
      FHead.foldRight(fa.head, z)((a, b) => FTail.foldRight(fa.tail, f(a, b))(f))

    override def foldLeft[A, B](fa: λ[A], z: B)(f: (B, A) => B) =
      FHead.foldLeft(fa.head, z)((b, a) => FTail.foldLeft(fa.tail, f(b, a))(f))

  }

  private[scalaz] trait ProductTraverse[F[_], T <: TCList]
    extends ProductFunctor[F, T]
    with ProductFoldable[F, T]
    with Traverse[TCCons[F, T]#Product]
    with Product[Traverse, F, T] {

    override def foldMap[A, B](fa: λ[A])(f: A => B)(implicit M: Monoid[B]) =
      super[ProductFoldable].foldMap(fa)(f)

    def traverseImpl[G[_], A, B](fa: λ[A])(f: A => G[B])(implicit G: Applicative[G]) =
      G.ap2(FHead.traverseImpl(fa.head)(f), FTail.traverseImpl(fa.tail)(f))(G.point(_ :: _))

  }

  private[scalaz] trait ProductDistributive[F[_], T <: TCList]
    extends ProductFunctor[F, T]
    with Distributive[TCCons[F, T]#Product]
    with Product[Distributive, F, T] {

    def distributeImpl[G[_] : Functor, A, B](ga: G[A])(f: A => λ[B]): λ[G[B]] =
      FHead.distribute(ga)(a => f(a).head) :: FTail.distribute(ga)(a => f(a).tail)

  }

  // Composition

  private[scalaz] trait Composed[+C[_[_]], F[_], T <: TCList] {
    def FOuter: C[F]
    def FInner: C[T#Composed]

    type FT[α] = F[T#Composed[α]]
  }

  private[scalaz] trait ComposedFunctor[F[_], T <: TCList]
    extends Functor[TCCons[F, T]#Composed]
    with Composed[Functor, F, T] {

    override def map[A, B](fa: FT[A])(f: A => B) =
      FOuter.map(fa)(ia => FInner.map(ia)(f))

  }

  private[scalaz] trait ComposedPointed[F[_], T <: TCList]
    extends ComposedFunctor[F, T]
    with Pointed[TCCons[F, T]#Composed]
    with Composed[Pointed, F, T] {

    def point[A](a: => A) =
      FOuter.point(FInner.point(a))

  }

  private[scalaz] trait ComposedApply[F[_], T <: TCList]
    extends ComposedFunctor[F, T]
    with Apply[TCCons[F, T]#Composed]
    with Composed[Apply, F, T] {

    def ap[A, B](fa: => FT[A])(f: => FT[A => B]) =
      FOuter.apply2(f, fa)((ff, ia) => FInner.ap(ia)(ff))

  }

  private[scalaz] trait ComposedApplicative[F[_], T <: TCList]
    extends ComposedPointed[F, T]
    with ComposedApply[F, T]
    with Applicative[TCCons[F, T]#Composed]
    with Composed[Applicative, F, T]

  private[scalaz] trait ComposedFoldable[F[_], T <: TCList]
    extends Foldable[TCCons[F, T]#Composed]
    with Composed[Foldable, F, T] {

    override def foldMap[A, B](fa: FT[A])(f: A => B)(implicit M: Monoid[B]) =
      FOuter.foldMap(fa)(FInner.foldMap(_)(f))

    override def foldRight[A, B](fa: FT[A], z: => B)(f: (A, => B) => B) =
      FOuter.foldRight(fa, z)((a, b) => FInner.foldRight(a, b)(f))

    override def foldLeft[A, B](fa: FT[A], z: B)(f: (B, A) => B) =
      FOuter.foldLeft(fa, z)((b, a) => FInner.foldLeft(a, b)(f))

  }

  private[scalaz] trait ComposedTraverse[F[_], T <: TCList]
    extends Traverse[TCCons[F, T]#Composed]
    with ComposedFunctor[F, T]
    with ComposedFoldable[F, T]
    with Composed[Traverse, F, T] {

    def traverseImpl[G[_], A, B](fa: FT[A])(f: A => G[B])(implicit G: Applicative[G]) =
      FOuter.traverse(fa)(FInner.traverse(_)(f))
  }

  private[scalaz] trait ComposedDistributive[F[_], T <: TCList]
    extends ComposedFunctor[F, T]
    with Distributive[TCCons[F, T]#Composed]
    with Composed[Distributive, F, T] {

    def distributeImpl[G[_] : Functor, A, B](ga: G[A])(f: A => FT[B]): FT[G[B]] =
      FOuter(FOuter.distribute(ga)(f))(FInner.cosequence(_))
  }

  // Empty product and composition

  private trait Empty {

    protected def _emptyProduct = new Applicative[TCNil#Product]
      with Traverse[TCNil#Product]
      with Distributive[TCNil#Product] {

      override def map[A, B](fa: HNil)(f: A => B) = HNil

      def point[A](a: => A) = HNil

      def ap[A, B](fa: => HNil)(f: => HNil) = HNil

      override def foldMap[A, B](fa: HNil)(f: A => B)(implicit F: Monoid[B]) = F.zero

      override def foldRight[A, B](fa: HNil, z: => B)(f: (A, => B) => B) = z

      def traverseImpl[G[_] : Applicative, A, B](fa: HNil)(g: A => G[B]) = Applicative[G].point(HNil)

      def distributeImpl[G[_] : Functor, A, B](ga: G[A])(f: A => HNil) = HNil

    }

    protected def _idCompose = new Applicative[TCNil#Composed]
      with Traverse[TCNil#Composed]
      with Distributive[TCNil#Composed] {

      override def map[A, B](fa: A)(f: A => B) = f(fa)

      def point[A](a: => A) = a

      def ap[A, B](fa: => A)(f: => A => B) = f(fa)

      override def foldMap[A, B](fa: A)(f: A => B)(implicit F: Monoid[B]) = f(fa)

      override def foldRight[A, B](fa: A, z: => B)(f: (A, => B) => B) = f(fa, z)

      def traverseImpl[G[_] : Applicative, A, B](fa: A)(g: A => G[B]) = g(fa)

      def distributeImpl[G[_] : Functor, A, B](ga: G[A])(f: A => B) = Functor[G].map(ga)(f)

    }

  }

  // Instances

  implicit def FunctorI: KTypeClass[Functor] = new KTypeClass[Functor] with Empty {
    def product[F[_], T <: TCList](FH: Functor[F], FT: Functor[T#Product]) =
      new ProductFunctor[F, T] { def FHead = FH; def FTail = FT }
    def compose[F[_], T <: TCList](FO: Functor[F], FI: Functor[T#Composed]) =
      new ComposedFunctor[F, T] { def FOuter = FO; def FInner = FI }
  }

  implicit def PointedI: KTypeClass[Pointed] = new KTypeClass[Pointed] with Empty {
    def product[F[_], T <: TCList](FH: Pointed[F], FT: Pointed[T#Product]) =
      new ProductPointed[F, T] { def FHead = FH; def FTail = FT }
    def compose[F[_], T <: TCList](FO: Pointed[F], FI: Pointed[T#Composed]) =
      new ComposedPointed[F, T] { def FOuter = FO; def FInner = FI }
  }

  implicit def ApplyI: KTypeClass[Apply] = new KTypeClass[Apply] with Empty {
    def product[F[_], T <: TCList](FH: Apply[F], FT: Apply[T#Product]) =
      new ProductApply[F, T] { def FHead = FH; def FTail = FT }
    def compose[F[_], T <: TCList](FO: Apply[F], FI: Apply[T#Composed]) =
      new ComposedApply[F, T] { def FOuter = FO; def FInner = FI }
  }

  implicit def ApplicativeI: KTypeClass[Applicative] = new KTypeClass[Applicative] with Empty {
    def product[F[_], T <: TCList](FH: Applicative[F], FT: Applicative[T#Product]) =
      new ProductApplicative[F, T] { def FHead = FH; def FTail = FT }
    def compose[F[_], T <: TCList](FO: Applicative[F], FI: Applicative[T#Composed]) =
      new ComposedApplicative[F, T] { def FOuter = FO; def FInner = FI }
  }

  implicit def FoldableI: KTypeClass[Foldable] = new KTypeClass[Foldable] with Empty {
    def product[F[_], T <: TCList](FH: Foldable[F], FT: Foldable[T#Product]) =
      new ProductFoldable[F, T] { def FHead = FH; def FTail = FT }
    def compose[F[_], T <: TCList](FO: Foldable[F], FI: Foldable[T#Composed]) =
      new ComposedFoldable[F, T] { def FOuter = FO; def FInner = FI }
  }

  implicit def TraverseI: KTypeClass[Traverse] = new KTypeClass[Traverse] with Empty {
    def product[F[_], T <: TCList](FH: Traverse[F], FT: Traverse[T#Product]) =
      new ProductTraverse[F, T] { def FHead = FH; def FTail = FT }
    def compose[F[_], T <: TCList](FO: Traverse[F], FI: Traverse[T#Composed]) =
      new ComposedTraverse[F, T] { def FOuter = FO; def FInner = FI }
  }

  implicit def DistributiveI: KTypeClass[Distributive] = new KTypeClass[Distributive] with Empty {
    def product[F[_], T <: TCList](FH: Distributive[F], FT: Distributive[T#Product]) =
      new ProductDistributive[F, T] { def FHead = FH; def FTail = FT }
    def compose[F[_], T <: TCList](FO: Distributive[F], FI: Distributive[T#Composed]) =
      new ComposedDistributive[F, T] { def FOuter = FO; def FInner = FI }
  }

}

// vim: expandtab:ts=2:sw=2
