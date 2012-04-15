package scalaz
package typelevel

import syntax.HLists._

/**
 * A structure storing a list of type constructors in the type, providing
 * type conversions for composition and products.
 *
 * @note There are no values of this type.
 * @see [[scalaz.typelevel.TypeClass]]
 */
sealed trait TCList {

  /**
   * The type of the product of all type constructors applied to a specific
   * type. In this case, "product" means a [[scalaz.typelevel.HList]].
   */
  type Product[α] <: HList

  /**
   * The type of the composition of the type constructors, applied to a
   * specific type. This can be seen as a right fold with initial element
   * `α` and type constructor application as the function.
   *
   * @note The first type constructor in this list is applied ''last''.
   */
  type Composed[α]

}

sealed trait TCCons[M[_], T <: TCList] extends TCList {
  override type Product[α] = M[α] :: T#Product[α]
  override type Composed[α] = M[T#Composed[α]]
}

sealed trait TCNil extends TCList {
  override type Product[α] = HNil
  type Composed[α] = α
}

// vim: expandtab:ts=2:sw=2
