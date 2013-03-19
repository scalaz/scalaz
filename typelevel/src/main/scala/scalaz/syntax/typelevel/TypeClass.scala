package scalaz
package syntax
package typelevel

import scalaz.typelevel._

trait TypeClasses {

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
