package scalaz
package syntax

trait Syntaxes {

  object semigroup extends ToSemigroupV

  object monoid extends ToMonoidV

  object group extends ToGroupV

  object equal extends ToEqualV

  object length extends ToLengthV

  object show extends ToShowV

  object order extends ToOrderV

  object enum extends ToEnumV

  object metricSpace extends ToMetricSpaceV

  object plusEmpty extends ToPlusEmptyV

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

  object compose extends ToComposeV

  object category extends ToCategoryV

  object arrId extends ToArrIdV

  object arrow extends ToArrowV

  object id extends ToIdV

  object tree extends ToTreeV

  object reducer extends ToReducerV

  object writer extends ToWriterV

  object foldable extends ToFoldableV

  object validation extends ToValidationV

  object kleisli extends ToKleisliV

  object all extends ToAllTypeClassV with ToAllOtherV

}

trait ToAllOtherV extends ToIdV with ToTreeV with ToWriterV with ToValidationV with ToReducerV with ToKleisliV

trait ToAllTypeClassV
  extends ToSemigroupV with ToMonoidV with ToGroupV with ToEqualV with ToLengthV with ToShowV
  with ToOrderV with ToEnumV with ToMetricSpaceV with ToPlusEmptyV with ToEachV with ToIndexV
  with ToFunctorV with ToPointedV with ToContravariantV with ToCopointedV with ToApplyV
  with ToApplicativeV with ToBindV with ToMonadV with ToCojoinV with ToComonadV
  with ToPlusV with ToApplicativePlusV with ToMonadPlusV with ToTraverseV with ToBiFunctorV
  with ToBiTraverseV with ToArrIdV with ToComposeV with ToCategoryV
  with ToArrowV with ToFoldableV


trait SyntaxV[A] {
  def self: A
}

/**The members of this object are also offered in the package object [[scalaz.syntax]] */
object Syntax extends Syntaxes
