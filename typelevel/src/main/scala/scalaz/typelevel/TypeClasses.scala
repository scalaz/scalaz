package scalaz
package typelevel

import syntax.HLists._

/**
 * A type class abstracting over the `product` and `compose` operations of type
 * classes.
 *
 * There are instances for [[scalaz.Functor]], [[scalaz.Pointed]] and
 * [[scalaz.Applicative]].
 *
 * Serves as a replacement of [[scalaz.Composition]].
 *
 * Composition and multiplication of instances is done via wrappers which are
 * unpacked automatically if the `unpack*` implicits of
 * [[scalaz.typelevel.syntax.TypeClasses]] are imported.
 *
 * @see [[scalaz.typelevel.syntax.TypeClasses]]
 * @see [[scalaz.typelevel.TCList]]
 */
trait TypeClass[C[_[_]]] {

  import TypeClass._

  /**
   * Given a type class instance for `F`, and a type class instance for a
   * product, produce a type class instance for the product prepended with `F`.
   *
   * @note Users of this class should start with `emptyProduct` and use the
   * functions on [[scalaz.typelevel.TypeClass.WrappedProduct]], which greatly
   * improves type inference.
   */
  def product[F[_], T <: TCList](FHead: C[F], FTail: C[T#Product]): C[TCCons[F, T]#Product]

  /**
   * Given a type class instance for `F`, and a type class instance for a
   * composition, produce a type class instance for the composition wrapped by
   * `F`.
   *
   * @note Users of this class should start with `idCompose` and use the
   * functions on [[scalaz.typelevel.TypeClass.WrappedComposition]], which greatly
   * improves type inference.
   */
  def compose[F[_], T <: TCList](FOuter: C[F], FInner: C[T#Composed]): C[TCCons[F, T]#Composed]

  // implement this function to return a type class performing identity operations on `HNil`
  protected def _emptyProduct: C[TCNil#Product]

  // implement this function to return the type class instance for `Id`
  protected def _idCompose: C[TCNil#Composed]

  /**
   * The empty product.
   *
   * @return an instance of [[scalaz.typelevel.TypeClass.WrappedProduct]]
   * representing an empty product
   */
  final def emptyProduct = new WrappedProduct[C, TCNil](_emptyProduct, this)

  /**
   * The empty composition.
   *
   * @return an instance of [[scalaz.typelevel.TypeClass.WrappedComposition]]
   * representing an empty composition
   */
  final def idCompose = new WrappedComposition[C, TCNil](_idCompose, this)

}

object TypeClass {

  @inline def apply[C[_[_]]](implicit C: TypeClass[C]) = C

  implicit def functorClass: TypeClass[Functor] = FunctorClass
  implicit def pointedClass: TypeClass[Pointed] = PointedClass
  implicit def applicativeClass: TypeClass[Applicative] = ApplicativeClass

  /** Wrapping the computation of a product. */
  class WrappedProduct[C[_[_]], T <: TCList](val instance: C[T#Product], typeClass: TypeClass[C]) {

    def *:[F[_]](F: C[F]) = new WrappedProduct[C, TCCons[F, T]](typeClass.product(F, instance), typeClass)

    def prodLeft[F[_]](F: C[F]) = F *: this

  }

  /** Wrapping the computation of a composition. */
  class WrappedComposition[C[_[_]], T <: TCList](val instance: C[T#Composed], typeClass: TypeClass[C]) {

    def <<:[F[_]](F: C[F]) = new WrappedComposition[C, TCCons[F, T]](typeClass.compose[F, T](F, instance), typeClass)

    def :>>[F[_]](F: C[F]) = F <<: this

    def composeInto[F[_]](F: C[F]) = F <<: this

  }

  /**
   * Providing `emptyProduct` and `idCompose` of [[scalaz.typelevel.TypeClass]]
   * on the companion object for the corresponding type classes.
   */
  class TypeClassCompanion[C[_[_]] : TypeClass]() {

    /** The empty product of `C`. */
    def product = TypeClass[C].emptyProduct

    /** The empty composition of `C`. */
    def compose = TypeClass[C].idCompose

  }


  // The following classes are in place mostly to avoid repetition of methods
  // of type classes which are inherited from some other ones.

  // Products

  private class ProductFunctor[F[_], T <: TCList](FHead: Functor[F], FTail: Functor[T#Product])
    extends Functor[TCCons[F, T]#Product] {

    def map[A, B](fa: F[A] :: T#Product[A])(f: A => B) =
      FHead.map(fa.head)(f) :: FTail.map(fa.tail)(f)

  }

  private class ProductPointed[F[_], T <: TCList](FHead: Pointed[F], FTail: Pointed[T#Product])
    extends ProductFunctor[F, T](FHead, FTail)
    with Pointed[TCCons[F, T]#Product] {

    def point[A](a: => A) =
      FHead.point(a) :: FTail.point(a)

  }

  private class ProductApplicative[F[_], T <: TCList](FHead: Applicative[F], FTail: Applicative[T#Product])
    extends ProductPointed[F, T](FHead, FTail)
    with Applicative[TCCons[F, T]#Product] {

    def ap[A, B](fa: => F[A] :: T#Product[A])(f: => F[A => B] :: T#Product[A => B]) =
      FHead.ap(fa.head)(f.head) :: FTail.ap(fa.tail)(f.tail)

  }

  // Composition

  private class ComposedFunctor[F[_], T <: TCList](FOuter: Functor[F], FInner: Functor[T#Composed])
    extends Functor[TCCons[F, T]#Composed] {

    def map[A, B](fa: F[T#Composed[A]])(f: A => B) =
      FOuter.map(fa)(ia => FInner.map(ia)(f))

  }

  private class ComposedPointed[F[_], T <: TCList](FOuter: Pointed[F], FInner: Pointed[T#Composed])
    extends ComposedFunctor[F, T](FOuter, FInner)
    with Pointed[TCCons[F, T]#Composed] {

    def point[A](a: => A) =
      FOuter.point(FInner.point(a))

  }

  private class ComposedApplicative[F[_], T <: TCList](FOuter: Applicative[F], FInner: Applicative[T#Composed])
    extends ComposedPointed[F, T](FOuter, FInner)
    with Applicative[TCCons[F, T]#Composed] {

    def ap[A, B](fa: => F[T#Composed[A]])(f: => F[T#Composed[A => B]]) =
      FOuter.map2(f, fa)((ff, ia) => FInner.ap(ia)(ff))

  }

  // Empty product and composition

  private trait Empty {
    protected def _emptyProduct: Applicative[TCNil#Product] = new Applicative[TCNil#Product] {
      override def map[A, B](fa: HNil)(f: A => B): HNil = HNil
      def point[A](a: => A): HNil = HNil
      def ap[A, B](fa: => HNil)(f: => HNil): HNil = HNil
    }

    protected def _idCompose: Applicative[TCNil#Composed] = new Applicative[TCNil#Composed] {
      override def map[A, B](fa: A)(f: A => B): B = f(fa)
      def point[A](a: => A): A = a
      def ap[A, B](fa: => A)(f: => A => B): B = f(fa)
    }
  }

  // TypeClass instances

  // these are not 'implicit objects', because the private type `Empty` would
  // escape its defining scope

  private object FunctorClass extends TypeClass[Functor] with Empty {

    def product[F[_], T <: TCList](FHead: Functor[F], FTail: Functor[T#Product]) =
      new ProductFunctor(FHead, FTail)

    def compose[F[_], T <: TCList](FOuter: Functor[F], FInner: Functor[T#Composed]) =
      new ComposedFunctor(FOuter, FInner)

  }

  private object PointedClass extends TypeClass[Pointed] with Empty {

    def product[F[_], T <: TCList](FHead: Pointed[F], FTail: Pointed[T#Product]) =
      new ProductPointed(FHead, FTail)

    def compose[F[_], T <: TCList](FOuter: Pointed[F], FInner: Pointed[T#Composed]) =
      new ComposedPointed(FOuter, FInner)

  }

  private object ApplicativeClass extends TypeClass[Applicative] with Empty {

    def product[F[_], T <: TCList](FHead: Applicative[F], FTail: Applicative[T#Product]) =
      new ProductApplicative(FHead, FTail)

    def compose[F[_], T <: TCList](FOuter: Applicative[F], FInner: Applicative[T#Composed]) =
      new ComposedApplicative(FOuter, FInner)

  }

}

// vim: expandtab:ts=2:sw=2
