package scalaz
package typelevel

import syntax.HLists._

/**
 * A type class abstracting over the `product` operation of type classes over
 * types of kind `*`.
 *
 * @see [[scalaz.typelevel.syntax.TypeClasses]]
 */
trait TypeClass[C[_]] {

  import TypeClass._

  /**
   * Given a type class instance for `F`, and a type class instance for a
   * product, produce a type class instance for the product prepended with `F`.
   */
  def product[F, T <: HList](FHead: C[F], FTail: C[T]): C[F :: T]

  /**
   * The empty product.
   */
  def emptyProduct: C[HNil]

  /** The product containing one element. */
  final def product1[F](implicit F: C[F]): C[F :: HNil] =
    product(F, emptyProduct)

  /**The product containing two elements. */
  final def product2[F, G](implicit F: C[F], G: C[G]): C[F :: G :: HNil] =
    product(F, product(G, emptyProduct))

  /**The product containing three elements. */
  final def product3[F, G, H](implicit F: C[F], G: C[G], H: C[H]): C[F :: G :: H :: HNil] =
    product(F, product(G, product(H, emptyProduct)))
}

object TypeClass {

  @inline def apply[C[_]](implicit C: TypeClass[C]) = C


  // Empty product

  private trait Empty {

    def emptyProduct = new Equal[HNil]
      with Show[HNil] with Group[HNil] {

      def zero = HNil

      def append(f1: HNil, f2: => HNil) = HNil

      def inverse(f: HNil) = HNil

      def equal(a1: HNil, a2: HNil) = true

      override def shows(f: HNil) = "HNil"

    }

  }

  // The following classes are in place mostly to avoid repetition of methods
  // of type classes which are inherited from some other ones.

  private trait Product[+C[_], F, T <: HList] {
    def FHead: C[F]
    def FTail: C[T]

    type λ = F :: T
  }

  private trait ProductSemigroup[F, T <: HList]
    extends Semigroup[F :: T]
    with Product[Semigroup, F, T] {

    def append(f1: λ, f2: => λ) =
      FHead.append(f1.head, f2.head) :: FTail.append(f1.tail, f2.tail)

  }

  private trait ProductMonoid[F, T <: HList]
    extends ProductSemigroup[F, T]
    with Monoid[F :: T]
    with Product[Monoid, F, T] {

    def zero = FHead.zero :: FTail.zero

  }

  private trait ProductGroup[F, T <: HList]
    extends ProductMonoid[F, T]
    with Group[F :: T]
    with Product[Group, F, T] {

    def inverse(f: λ) =
      FHead.inverse(f.head) :: FTail.inverse(f.tail)

  }

  // Instances

  implicit def EqualI: TypeClass[Equal] = new TypeClass[Equal] with Empty {

    def product[F, T <: HList](FHead: Equal[F], FTail: Equal[T]) = new Equal[F :: T] {
      def equal(a1: F :: T, a2: F :: T) = FHead.equal(a1.head, a2.head) && FTail.equal(a1.tail, a2.tail)
    }

  }

  implicit def ShowI: TypeClass[Show] = new TypeClass[Show] with Empty {

    import scalaz.std.string.stringInstance
    import formatters.string.Strings

    def product[F, T <: HList](FHead: Show[F], FTail: Show[T]) = new Show[F :: T] {
      override def shows(f: F :: T) = (Strings.show[String] :: " :: " :: Strings.show[String] :: FNil) format (FHead.shows(f.head) :: FTail.shows(f.tail) :: HNil)
    }

  }

  implicit def SemigroupI: TypeClass[Semigroup] = new TypeClass[Semigroup] with Empty {
    def product[F, T <: HList](FH: Semigroup[F], FT: Semigroup[T]): Semigroup[F :: T] =
      new ProductSemigroup[F, T] { def FHead = FH; def FTail = FT }
  }

  implicit def MonoidI: TypeClass[Monoid] = new TypeClass[Monoid] with Empty {
    def product[F, T <: HList](FH: Monoid[F], FT: Monoid[T]): Monoid[F :: T] =
      new ProductMonoid[F, T] { def FHead = FH; def FTail = FT }
  }

  implicit def GroupI: TypeClass[Group] = new TypeClass[Group] with Empty {
    def product[F, T <: HList](FH: Group[F], FT: Group[T]): Group[F :: T] =
      new ProductGroup[F, T] { def FHead = FH; def FTail = FT }
  }

}


/*

// this should work, but it crashes the compiler (2.9.2)

trait TypeClasses {

  implicit def nilInstance[C[_] : TypeClass]: C[HNil] = TypeClass[C].emptyProduct

  implicit def consInstance[C[_], H, T <: HList](implicit C: TypeClass[C], H: C[H], T: C[T]): C[H :: T] =
    C.product(H, T)

}

object TypeClasses extends TypeClasses
*/

// vim: expandtab:ts=2:sw=2
