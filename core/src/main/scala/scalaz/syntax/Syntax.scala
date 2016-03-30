package scalaz
package syntax

trait Syntaxes {

  //
  // Type classes over * -> *
  //

  object semigroup extends ToSemigroupOps

  object monoid extends ToMonoidOps

  object equal extends ToEqualOps

  object show extends ToShowOps

  object order extends ToOrderOps

  object enum extends ToEnumOps

  object isEmpty extends ToIsEmptyOps

  object plusEmpty extends ToPlusEmptyOps

  object functor extends ToFunctorOps

  object invariantFunctor extends ToInvariantFunctorOps

  object contravariant extends ToContravariantOps

  object align extends ToAlignOps

  object apply extends ToApplyOps

  object applicative extends ToApplicativeOps

  object bind extends ToBindOps

  object monad extends ToMonadOps

  object cobind extends ToCobindOps

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

  object catchable extends ToCatchableOps

  //
  // Type classes over * * -> *
  //

  object associative extends ToAssociativeOps

  object bifunctor extends ToBifunctorOps

  object bifoldable extends ToBifoldableOps

  object bitraverse extends ToBitraverseOps

  object compose extends ToComposeOps

  object profunctor extends ToProfunctorOps

  object strong extends ToStrongOps

  object proChoice extends ToProChoiceOps

  object category extends ToCategoryOps

  object arrow extends ToArrowOps

  object choice extends ToChoiceOps

  object split extends ToSplitOps

  object monadTell extends ToMonadTellOps

  object monadListen extends ToMonadListenOps

  object monadError extends ToMonadErrorOps

  //
  // Data
  //

  object id extends ToIdOps

  object tree extends ToTreeOps

  object strictTree extends ToStrictTreeOps

  object reducer extends ToReducerOps

  object writer extends ToWriterOps

  object state extends ToStateOps

  object validation extends ToValidationOps

  object kleisli extends ToKleisliOps

  object either extends ToEitherOps

  object nel extends ToNelOps

  object these extends ToTheseOps

  object maybe extends ToMaybeOps

  object tag extends ToTagOps

  //
  // Mixed
  //

  object all extends ToTypeClassOps with ToDataOps

}

trait ToDataOps
  extends ToIdOps
  with ToTreeOps
  with ToStrictTreeOps
  with ToReducerOps
  with ToWriterOps
  with ToStateOps
  with ToValidationOps
  with ToKleisliOps
  with ToEitherOps
  with ToNelOps
  with ToTheseOps
  with ToMaybeOps

trait ToTypeClassOps
  extends ToSemigroupOps with ToMonoidOps with ToEqualOps with ToShowOps
  with ToOrderOps with ToEnumOps with ToPlusEmptyOps
  with ToFunctorOps with ToContravariantOps with ToApplyOps
  with ToApplicativeOps with ToBindOps with ToMonadOps with ToComonadOps
  with ToBifoldableOps with ToCozipOps
  with ToPlusOps with ToApplicativePlusOps with ToMonadPlusOps with ToTraverseOps with ToBifunctorOps with ToAssociativeOps
  with ToBitraverseOps with ToComposeOps with ToCategoryOps
  with ToArrowOps with ToFoldableOps with ToChoiceOps with ToSplitOps with ToZipOps with ToUnzipOps with ToMonadTellOps with ToMonadListenOps with ToMonadErrorOps
  with ToFoldable1Ops with ToTraverse1Ops with ToOptionalOps with ToCatchableOps with ToAlignOps
