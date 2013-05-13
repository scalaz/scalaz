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

  object index extends ToIndexOps

  object functor extends ToFunctorOps

  object contravariant extends ToContravariantOps

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

  object traverse1 extends ToTraverse1Ops

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

  object foldable extends ToFoldableOps

  object foldable1 extends ToFoldable1Ops

  object validation extends ToValidationOps

  object kleisli extends ToKleisliOps

  //
  // Mixed
  //

  object all extends ToTypeClassOps with ToDataOps

}

trait ToDataOps extends ToIdOps with ToTreeOps with ToWriterOps with ToValidationOps with ToReducerOps with ToKleisliOps

trait ToTypeClassOps
  extends ToSemigroupOps with ToMonoidOps with ToEqualOps with ToShowOps
  with ToOrderOps with ToEnumOps with ToPlusEmptyOps with ToIndexOps
  with ToFunctorOps with ToContravariantOps with ToApplyOps
  with ToApplicativeOps with ToBindOps with ToMonadOps with ToCojoinOps with ToComonadOps
  with ToBifoldableOps with ToCozipOps
  with ToPlusOps with ToApplicativePlusOps with ToMonadPlusOps with ToTraverseOps with ToBifunctorOps
  with ToBitraverseOps with ToComposeOps with ToCategoryOps
  with ToArrowOps with ToFoldableOps with ToChoiceOps with ToSplitOps with ToZipOps with ToUnzipOps with ToMonadTellOps with ToMonadListenOps
  with ToFoldable1Ops with ToTraverse1Ops
