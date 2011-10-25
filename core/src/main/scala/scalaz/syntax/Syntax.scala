package scalaz
package syntax

import std.{ToStreamV, ToOptionV}

trait Syntaxes {

  object semigroup extends ToSemigroupV

  object monoid extends ToMonoidV

  object equal extends ToEqualV

  object length extends ToLengthV

  object show extends ToShowV

  object order extends ToOrderV

  object metricSpace extends ToMetricSpaceV

  object empty extends ToEmptyV

  object each extends ToEachV

  object index extends ToIndexV

  object functor extends ToFunctorV

  object pointed extends ToPointedV

  object contravariant extends ToContravariantV

  object copointed extends ToCopointedV

  object apply extends ToApplyV

  object applicative extends ToApplicativeV

  object bind extends ToBindV

  object monad extends ToMonadV

  object cojoin extends ToCojoinV

  object comonad extends ToComonadV

  object plus extends ToPlusV

  object applicativePlus extends ToApplicativePlusV

  object monadPlus extends ToMonadPlusV

  object traverse extends ToTraverseV

  object biFunctor extends ToBiFunctorV

  object biTraverse extends ToBiTraverseV

  object arrId extends ToArrIdV

  object arr extends ToArrV

  object compose extends ToComposeV

  object category extends ToCategoryV

  object first extends ToFirstV

  object arrow extends ToArrowV

  object all extends ToAllTypeClassV

}

trait ToAllTypeClassV
  extends ToSemigroupV with ToMonoidV with ToEqualV with ToLengthV with ToShowV
  with ToOrderV with ToMetricSpaceV with ToEmptyV with ToEachV with ToIndexV
  with ToFunctorV with ToPointedV with ToContravariantV with ToCopointedV with ToApplyV
  with ToApplicativeV with ToBindV with ToMonadV with ToCojoinV with ToComonadV
  with ToPlusV with ToApplicativePlusV with ToMonadPlusV with ToTraverseV with ToBiFunctorV
  with ToBiTraverseV with ToArrIdV with ToArrV with ToComposeV with ToCategoryV
  with ToFirstV with ToArrowV

trait ToAllStdV
  extends ToOptionV with ToStreamV


trait SyntaxV[A] {
  def self: A
}

/**The members of this object are also offered in the package object [[scalaz.syntax]] */
object Syntax extends Syntaxes
