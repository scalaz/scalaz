package scalaz
package syntax

trait Syntaxes {

  //
  // Type classes over * -> *
  //

  object semigroup extends ToSemigroupOps

  object monoid extends ToMonoidOps

  object equal extends ToEqualOps

  @deprecated("length is deprecated, use foldable instead", "7.1")
  object length extends ToLengthOps

  object show extends ToShowOps

  object order extends ToOrderOps

  object enum extends ToEnumOps

  @deprecated("metricSpace is deprecated", "7.0.1")
  object metricSpace extends ToMetricSpaceOps

  object isEmpty extends ToIsEmptyOps

  object plusEmpty extends ToPlusEmptyOps

  @deprecated("each is deprecated", "7.1")
  object each extends ToEachOps

  @deprecated("index is deprecated", "7.1")
  object index extends ToIndexOps

  object functor extends ToFunctorOps

  object invariantFunctor extends ToInvariantFunctorOps

  object contravariant extends ToContravariantOps

  object apply extends ToApplyOps

  object applicative extends ToApplicativeOps

  object bind extends ToBindOps

  object monad extends ToMonadOps

  @deprecated("cojoin has been merged into cobind", "7.1")
  object cojoin extends ToCobindOps

  object comonad extends ToComonadOps

  object cozip extends ToCozipOps

  object plus extends ToPlusOps

  object applicativePlus extends ToApplicativePlusOps

  object monadPlus extends ToMonadPlusOps

  object foldable extends ToFoldableOps

  object foldable1 extends ToFoldable1Ops

  object traverse extends ToTraverseOps

  object traverse1 extends ToTraverse1Ops

  object zip extends ToZipOps

  object unzip extends ToUnzipOps

  object optional extends ToOptionalOps

  //
  // Type classes over * * -> *
  //

  object bifunctor extends ToBifunctorOps

  object bifoldable extends ToBifoldableOps

  object bitraverse extends ToBitraverseOps

  object compose extends ToComposeOps

  object category extends ToCategoryOps

  object arrow extends ToArrowOps

  object choice extends ToChoiceOps

  object split extends ToSplitOps

  object monadTell extends ToMonadTellOps

  object monadListen extends ToMonadListenOps

  //
  // Data
  //

  object id extends ToIdOps

  object tree extends ToTreeOps

  object reducer extends ToReducerOps

  object writer extends ToWriterOps

  object state extends ToStateOps

  object validation extends ToValidationOps

  object kleisli extends ToKleisliOps

  object either extends ToEitherOps

  object nel extends ToNelOps

  //
  // Mixed
  //

  object all extends ToTypeClassOps with ToDataOps

}

trait ToDataOps
  extends ToIdOps
  with ToTreeOps
  with ToReducerOps
  with ToWriterOps
  with ToStateOps
  with ToValidationOps
  with ToKleisliOps
  // TODO those are not yet included because of ambiguities when importing all syntax
  // can be included again when the @deprecated methods are being removed
  // with ToEitherOps
  // with ToNelOps

trait ToTypeClassOps
  extends ToSemigroupOps with ToMonoidOps with ToEqualOps with ToLengthOps with ToShowOps
  with ToOrderOps with ToEnumOps with ToMetricSpaceOps with ToPlusEmptyOps with ToEachOps with ToIndexOps
  with ToFunctorOps with ToContravariantOps with ToApplyOps
  with ToApplicativeOps with ToBindOps with ToMonadOps with ToComonadOps
  with ToBifoldableOps with ToCozipOps
  with ToPlusOps with ToApplicativePlusOps with ToMonadPlusOps with ToTraverseOps with ToBifunctorOps
  with ToBitraverseOps with ToComposeOps with ToCategoryOps
  with ToArrowOps with ToFoldableOps with ToChoiceOps with ToSplitOps with ToZipOps with ToUnzipOps with ToMonadTellOps with ToMonadListenOps
  with ToFoldable1Ops with ToTraverse1Ops with ToOptionalOps
