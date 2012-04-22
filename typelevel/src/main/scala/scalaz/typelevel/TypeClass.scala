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

}

object TypeClass {

  @inline def apply[C[_]](implicit C: TypeClass[C]) = C


  // Empty product

  private trait Empty {

    def emptyProduct = new Equal[HNil]
      with Show[HNil] {

      def equal(a1: HNil, a2: HNil) = true

      def show(f: HNil) = shows(f).toList

      override def shows(f: HNil) = "HNil"

    }

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
      def show(f: F :: T) = shows(f).toList
      override def shows(f: F :: T) = (Strings.show[String] :: " :: " :: Strings.show[String] :: FNil) format (FHead.shows(f.head) :: FTail.shows(f.tail) :: HNil)
    }

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
