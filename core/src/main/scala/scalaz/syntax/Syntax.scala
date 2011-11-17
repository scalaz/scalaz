package scalaz
package syntax

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

  object coPointed extends ToCoPointedV

  object apply extends ToApplyV

  object applicative extends ToApplicativeV

  object bind extends ToBindV

  object monad extends ToMonadV

  object coJoin extends ToCoJoinV

  object coMonad extends ToCoMonadV

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

  object tree extends ToTreeV

  object writer extends ToWriterV

  object all extends ToAllTypeClassV with ToAllOtherV
}

trait ToAllOtherV extends ToTreeV with ToWriterV

trait ToAllTypeClassV
  extends ToSemigroupV with ToMonoidV with ToEqualV with ToLengthV with ToShowV
  with ToOrderV with ToMetricSpaceV with ToEmptyV with ToEachV with ToIndexV
  with ToFunctorV with ToPointedV with ToContravariantV with ToCoPointedV with ToApplyV
  with ToApplicativeV with ToBindV with ToMonadV with ToCoJoinV with ToCoMonadV
  with ToPlusV with ToApplicativePlusV with ToMonadPlusV with ToTraverseV with ToBiFunctorV
  with ToBiTraverseV with ToArrIdV with ToArrV with ToComposeV with ToCategoryV
  with ToFirstV with ToArrowV


trait SyntaxV[A] {
  def self: A
}

/**The members of this object are also offered in the package object [[scalaz.syntax]] */
object Syntax extends Syntaxes
