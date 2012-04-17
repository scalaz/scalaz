package scalaz
package typelevel
package syntax

trait TypeClasses {

  import TypeClass._

  // Unpack wrappers

  implicit def unpackProduct[C[_[_]], T <: TCList](wrapper: WrappedProduct[C, T]) = wrapper.instance

  implicit def unpackComposition[C[_[_]], T <: TCList](wrapper: WrappedComposition[C, T]) = wrapper.instance

  // Wrap companions

  implicit def wrapFunctor(functor: Functor.type) = new TypeClassCompanion[Functor]
  implicit def wrapPointed(pointed: Pointed.type) = new TypeClassCompanion[Pointed]
  implicit def wrapApplicative(applicative: Applicative.type) = new TypeClassCompanion[Applicative]

}

object TypeClasses extends TypeClasses

// vim: expandtab:ts=2:sw=2
