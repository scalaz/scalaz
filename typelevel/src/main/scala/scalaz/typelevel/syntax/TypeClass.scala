package scalaz
package typelevel
package syntax

final class TCOps[C[_], T <: HList](typeClass: TypeClass[C], instance: C[T]) {
  def *:[F](F: C[F]): C[HCons[F, T]] = typeClass.product(F, instance)
}

trait TypeClasses0 {

  implicit def ToTCOps[C[_], F](F: C[F])(implicit C: TypeClass[C]): TCOps[C, HCons[F, HNil]] =
    new TCOps[C, HCons[F, HNil]](C, C.product(F, C.emptyProduct))

}

trait TypeClasses extends TypeClasses0 {

  // Kind *

  implicit def ToTCOpsCons[C[_] : TypeClass, F, T <: HList](instance: C[T]): TCOps[C, T] =
    new TCOps[C, T](TypeClass[C], instance)

  // Kind * -> *

  import KTypeClass._

  // Unpack wrapper

  implicit def unpackKProduct[C[_[_]], T <: TCList](wrapper: WrappedProduct[C, T]) =
    wrapper.instance

  implicit def unpackKComposition[C[_[_]], T <: TCList](wrapper: WrappedComposition[C, T]) =
    wrapper.instance

  // Instance syntax

  implicit def wrapKProduct[C[_[_]] : KTypeClass, F[_]](instance: C[F]) =
    instance *: KTypeClass[C].emptyProduct

  implicit def wrapKComposition[C[_[_]] : KTypeClass, F[_]](instance: C[F]) =
    instance <<: KTypeClass[C].idCompose
}

// vim: expandtab:ts=2:sw=2
