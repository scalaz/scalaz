package scalaz
package typelevel
package syntax

trait TCOps[C[_], T <: HList] {

  def instance: TypeClass[C]

  def typeClass: C[T]

  def *:[F](F: C[F]): C[F :: T] = instance.product(F, typeClass)

}

/*

// 'illegal cyclic reference involving trait TypeClasses' if uncommented

trait TypeClasses0 {

  implicit def ToTCOps[C[_], F](F: C[F])(implicit C: TypeClass[C]): TCOps[C, F :: HNil] = new TCOps[C, F :: HNil] {
    val instance = C
    val typeClass = C.product(F, C.emptyProduct)
  }

}
*/

trait TypeClasses /* extends TypeClasses0 */ {

  // Kind *

  implicit def ToTCOpsCons[C[_] : TypeClass, F, T <: HList](tc: C[T]): TCOps[C, T] = new TCOps[C, T] {
    val instance = TypeClass[C]
    val typeClass = tc
  }


  // Kind * -> *

  import KTypeClass._

  // Unpack wrapper

  implicit def unpackKProduct[C[_[_]], T <: TCList](wrapper: WrappedProduct[C, T]) = wrapper.instance

  // Instance syntax

  implicit def wrapKProduct[C[_[_]] : KTypeClass, F[_]](instance: C[F]) = instance *: KTypeClass[C].emptyProduct

}

object TypeClasses extends TypeClasses

// vim: expandtab:ts=2:sw=2
