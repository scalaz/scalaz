package scalaz
package typelevel
package syntax

trait TypeClasses {

  import TypeClass._

  // Unpack wrapper

  implicit def unpackProduct[C[_[_]], T <: TCList](wrapper: WrappedProduct[C, T]) = wrapper.instance

  // Instance syntax

  implicit def wrapProduct[C[_[_]] : TypeClass, F[_]](instance: C[F]) = instance *: TypeClass[C].emptyProduct

}

object TypeClasses extends TypeClasses

// vim: expandtab:ts=2:sw=2
