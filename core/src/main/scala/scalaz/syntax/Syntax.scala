package scalaz
package syntax

trait Syntaxes {
  //
  // Type classes over * -> *
  //


  object semigroup extends ToSemigroupOps

  object monoid extends ToMonoidOps

  object group extends ToGroupOps

  object equal extends ToEqualOps

  object length extends ToLengthOps

  object show extends ToShowOps

  object order extends ToOrderOps

  object enum extends ToEnumOps

  object metricSpace extends ToMetricSpaceOps

  object plusEmpty extends ToPlusEmptyOps

  object each extends ToEachOps

  object index extends ToIndexOps

  object functor extends ToFunctorOps

  object pointed extends ToPointedOps

  object contravariant extends ToContravariantOps

  object copointed extends ToCopointedOps

  object apply extends ToApplyOps

  object applicative extends ToApplicativeOps

  object bind extends ToBindOps

  object monad extends ToMonadOps

  object cojoin extends ToCojoinOps

  object comonad extends ToComonadOps

  object cozip extends ToCozipOps

  object plus extends ToPlusOps

  object applicativePlus extends ToApplicativePlusOps

  object monadPlus extends ToMonadPlusOps

  object traverse extends ToTraverseOps

  object zip extends ToZipOps

  object unzip extends ToUnzipOps

  //
  // Type classes over * * -> *
  //

  object bifunctor extends ToBifunctorOps

  object bifoldable extends ToBifoldableOps

  object bitraverse extends ToBitraverseOps

  object compose extends ToComposeOps

  object category extends ToCategoryOps

  object arrId extends ToArrIdOps

  object arrow extends ToArrowOps

  object choice extends ToChoiceOps

  object split extends ToSplitOps

  object monadWriter extends ToMonadWriterOps

  object listenableMonadWriter extends ToListenableMonadWriterOps
  //
  // Data
  //

  object id extends ToIdOps

  object tree extends ToTreeOps

  object reducer extends ToReducerOps

  object writer extends ToWriterOps

  object state extends ToStateOps

  object foldable extends ToFoldableOps

  object validation extends ToValidationOps

  object kleisli extends ToKleisliOps

  //
  // Mixed
  //

  object all extends ToTypeClassOps with ToDataOps

}

trait ToDataOps extends ToIdOps with ToTreeOps with ToWriterOps with ToValidationOps with ToReducerOps with ToKleisliOps

trait ToTypeClassOps
  extends ToSemigroupOps with ToMonoidOps with ToGroupOps with ToEqualOps with ToLengthOps with ToShowOps
  with ToOrderOps with ToEnumOps with ToMetricSpaceOps with ToPlusEmptyOps with ToEachOps with ToIndexOps
  with ToFunctorOps with ToPointedOps with ToContravariantOps with ToCopointedOps with ToApplyOps
  with ToApplicativeOps with ToBindOps with ToMonadOps with ToCojoinOps with ToComonadOps
  with ToBifoldableOps with ToCozipOps
  with ToPlusOps with ToApplicativePlusOps with ToMonadPlusOps with ToTraverseOps with ToBifunctorOps
  with ToBitraverseOps with ToArrIdOps with ToComposeOps with ToCategoryOps
  with ToArrowOps with ToFoldableOps with ToChoiceOps with ToSplitOps with ToZipOps with ToUnzipOps with ToMonadWriterOps with ToListenableMonadWriterOps
