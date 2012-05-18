package scalaz
package typelevel

import syntax.HLists._

/**
 * A type class abstracting over the `product` operation of type classes over
 * types of kind `* -> *`.
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

  // implement this function to return a type class performing identity operations on `HNil`
  protected def _emptyProduct: C[TCNil#Product]

  /**
   * The empty product.
   *
   * @return an instance of [[scalaz.typelevel.KTypeClass.WrappedProduct]]
   * representing an empty product
   */
  final def emptyProduct = new WrappedProduct[C, TCNil](_emptyProduct, this)

}

object KTypeClass {

  @inline def apply[C[_[_]]](implicit C: KTypeClass[C]) = C

  /** Wrapping the computation of a product. */
  class WrappedProduct[C[_[_]], T <: TCList](val instance: C[T#Product], typeClass: KTypeClass[C]) {

    def *:[F[_]](F: C[F]) = new WrappedProduct[C, TCCons[F, T]](typeClass.product(F, instance), typeClass)

    def prodLeft[F[_]](F: C[F]) = F *: this

  }

  // The following classes are in place mostly to avoid repetition of methods
  // of type classes which are inherited from some other ones.

  private trait Product[+C[_[_]], F[_], T <: TCList] {
    def FHead: C[F]
    def FTail: C[T#Product]

    type λ[α] = F[α] :: T#Product[α]
  }

  private trait ProductFunctor[F[_], T <: TCList]
    extends Functor[TCCons[F, T]#Product]
    with Product[Functor, F, T] {

    def map[A, B](fa: λ[A])(f: A => B) =
      FHead.map(fa.head)(f) :: FTail.map(fa.tail)(f)

  }

  private trait ProductPointed[F[_], T <: TCList]
    extends ProductFunctor[F, T]
    with Pointed[TCCons[F, T]#Product]
    with Product[Pointed, F, T] {

    def point[A](a: => A) =
      FHead.point(a) :: FTail.point(a)

  }

  private trait ProductApply[F[_], T <: TCList]
    extends ProductFunctor[F, T]
    with Apply[TCCons[F, T]#Product]
    with Product[Apply, F, T] {

    def ap[A, B](fa: => λ[A])(f: => λ[A => B]) =
      FHead.ap(fa.head)(f.head) :: FTail.ap(fa.tail)(f.tail)

  }

  private trait ProductApplicative[F[_], T <: TCList]
    extends ProductPointed[F, T]
    with ProductApply[F, T]
    with Applicative[TCCons[F, T]#Product]
    with Product[Applicative, F, T]

  private trait ProductPlus[F[_], T <: TCList]
    extends Plus[TCCons[F, T]#Product]
    with Product[Plus, F, T] {

    def plus[A](a: λ[A], b: => λ[A]) =
      FHead.plus(a.head, b.head) :: FTail.plus(a.tail, b.tail)

  }

  private trait ProductPlusEmpty[F[_], T <: TCList]
    extends ProductPlus[F, T]
    with PlusEmpty[TCCons[F, T]#Product]
    with Product[PlusEmpty, F, T] {

    def empty[A] = FHead.empty[A] :: FTail.empty[A]

  }

  private trait ProductApplicativePlus[F[_], T <: TCList]
    extends ProductPlusEmpty[F, T]
    with ProductApplicative[F, T]
    with ApplicativePlus[TCCons[F, T]#Product]
    with Product[ApplicativePlus, F, T]

  private trait ProductFoldable[F[_], T <: TCList]
    extends Foldable[TCCons[F, T]#Product]
    with Product[Foldable, F, T] {

    def foldMap[A, B](fa: λ[A])(f: A => B)(implicit M: Monoid[B]) =
      M.append(FHead.foldMap(fa.head)(f), FTail.foldMap(fa.tail)(f))

    def foldRight[A, B](fa: λ[A], z: => B)(f: (A, => B) => B) =
      FHead.foldRight(fa.head, z)((a, b) => FTail.foldRight(fa.tail, f(a, b))(f))

    override def foldLeft[A, B](fa: λ[A], z: B)(f: (B, A) => B) =
      FHead.foldLeft(fa.head, z)((b, a) => FTail.foldLeft(fa.tail, f(b, a))(f))

  }

  private trait ProductTraverse[F[_], T <: TCList]
    extends ProductFunctor[F, T]
    with ProductFoldable[F, T]
    with Traverse[TCCons[F, T]#Product]
    with Product[Traverse, F, T] {

    override def foldMap[A, B](fa: λ[A])(f: A => B)(implicit M: Monoid[B]) =
      super[ProductFoldable].foldMap(fa)(f)

    def traverseImpl[G[_], A, B](fa: λ[A])(f: A => G[B])(implicit G: Applicative[G]) =
      G.ap2(FHead.traverseImpl(fa.head)(f), FTail.traverseImpl(fa.tail)(f))(G.point(_ :: _))

  }

  private trait ProductDistributive[F[_], T <: TCList]
    extends ProductFunctor[F, T]
    with Distributive[TCCons[F, T]#Product]
    with Product[Distributive, F, T] {

    def distributeImpl[G[_] : Functor, A, B](ga: G[A])(f: A => λ[B]): λ[G[B]] =
      FHead.distribute(ga)(a => f(a).head) :: FTail.distribute(ga)(a => f(a).tail)

  }

  private trait ProductZip[F[_], T <: TCList]
    extends Zip[TCCons[F, T]#Product]
    with Product[Zip, F, T] {

    def zip[A, B](a: => λ[A], b: => λ[B]) =
      FHead.zip(a.head, b.head) :: FTail.zip(a.tail, b.tail)

  }

  private trait ProductUnzip[F[_], T <: TCList]
    extends Unzip[TCCons[F, T]#Product]
    with Product[Unzip, F, T] {

    def unzip[A, B](x: λ[(A, B)]) = {
      val (ha, hb) = FHead.unzip(x.head)
      val (ta, tb) = FTail.unzip(x.tail)
      (ha :: ta, hb :: tb)
    }

  }

  // Empty product

  private trait Empty {

    protected def _emptyProduct = new ApplicativePlus[TCNil#Product]
      with Traverse[TCNil#Product]
      with Distributive[TCNil#Product]
      with Zip[TCNil#Product]
      with Unzip[TCNil#Product] {

      override def map[A, B](fa: HNil)(f: A => B) = HNil

      def point[A](a: => A) = HNil

      def ap[A, B](fa: => HNil)(f: => HNil) = HNil

      def plus[A](a: HNil, b: => HNil) = HNil

      def empty[A] = HNil

      override def foldMap[A, B](fa: HNil)(f: A => B)(implicit F: Monoid[B]) = F.zero

      override def foldRight[A, B](fa: HNil, z: => B)(f: (A, => B) => B) = z

      def traverseImpl[G[_] : Applicative, A, B](fa: HNil)(g: A => G[B]) = Applicative[G].point(HNil)

      def distributeImpl[G[_] : Functor, A, B](ga: G[A])(f: A => HNil) = HNil

      def zip[A, B](a: => HNil, b: => HNil) = HNil

      def unzip[A, B](a: HNil) = (HNil, HNil)

    }

  }

  // KTypeClass instances

  implicit def FunctorI: KTypeClass[Functor] = new KTypeClass[Functor] with Empty {
    def product[F[_], T <: TCList](FH: Functor[F], FT: Functor[T#Product]) =
      new ProductFunctor[F, T] { def FHead = FH; def FTail = FT }
  }

  implicit def PointedI: KTypeClass[Pointed] = new KTypeClass[Pointed] with Empty {
    def product[F[_], T <: TCList](FH: Pointed[F], FT: Pointed[T#Product]) =
      new ProductPointed[F, T] { def FHead = FH; def FTail = FT }
  }

  implicit def ApplyI: KTypeClass[Apply] = new KTypeClass[Apply] with Empty {
    def product[F[_], T <: TCList](FH: Apply[F], FT: Apply[T#Product]) =
      new ProductApply[F, T] { def FHead = FH; def FTail = FT }
  }

  implicit def ApplicativeI: KTypeClass[Applicative] = new KTypeClass[Applicative] with Empty {
    def product[F[_], T <: TCList](FH: Applicative[F], FT: Applicative[T#Product]) =
      new ProductApplicative[F, T] { def FHead = FH; def FTail = FT }
  }

  implicit def PlusI: KTypeClass[Plus] = new KTypeClass[Plus] with Empty {
    def product[F[_], T <: TCList](FH: Plus[F], FT: Plus[T#Product]) =
      new ProductPlus[F, T] { def FHead = FH; def FTail = FT }
  }

  implicit def PlusEmptyI: KTypeClass[PlusEmpty] = new KTypeClass[PlusEmpty] with Empty {
    def product[F[_], T <: TCList](FH: PlusEmpty[F], FT: PlusEmpty[T#Product]) =
      new ProductPlusEmpty[F, T] { def FHead = FH; def FTail = FT }
  }

  implicit def ApplicativePlusI: KTypeClass[ApplicativePlus] = new KTypeClass[ApplicativePlus] with Empty {
    def product[F[_], T <: TCList](FH: ApplicativePlus[F], FT: ApplicativePlus[T#Product]) =
      new ProductApplicativePlus[F, T] { def FHead = FH; def FTail = FT }
  }

  implicit def FoldableI: KTypeClass[Foldable] = new KTypeClass[Foldable] with Empty {
    def product[F[_], T <: TCList](FH: Foldable[F], FT: Foldable[T#Product]) =
      new ProductFoldable[F, T] { def FHead = FH; def FTail = FT }
  }

  implicit def TraverseI: KTypeClass[Traverse] = new KTypeClass[Traverse] with Empty {
    def product[F[_], T <: TCList](FH: Traverse[F], FT: Traverse[T#Product]) =
      new ProductTraverse[F, T] { def FHead = FH; def FTail = FT }
  }

  implicit def DistributiveI: KTypeClass[Distributive] = new KTypeClass[Distributive] with Empty {
    def product[F[_], T <: TCList](FH: Distributive[F], FT: Distributive[T#Product]) =
      new ProductDistributive[F, T] { def FHead = FH; def FTail = FT }
  }

  implicit def ZipI: KTypeClass[Zip] = new KTypeClass[Zip] with Empty {
    def product[F[_], T <: TCList](FH: Zip[F], FT: Zip[T#Product]) =
      new ProductZip[F, T] { def FHead = FH; def FTail = FT }
  }

  implicit def UnzipI: KTypeClass[Unzip] = new KTypeClass[Unzip] with Empty {
    def product[F[_], T <: TCList](FH: Unzip[F], FT: Unzip[T#Product]) =
      new ProductUnzip[F, T] { def FHead = FH; def FTail = FT }
  }

}

// vim: expandtab:ts=2:sw=2
