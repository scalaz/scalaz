package scalaz
package syntax

trait Syntaxes {

  //
  // Type classes over * -> *
  //

  object semigroup extends ToSemigroupOps

  object monoid extends ToMonoidOps

  object band extends ToBandOps

  object equal extends ToEqualOps

  object show extends ToShowOps

  object order extends ToOrderOps

  object enum extends ToEnumOps

  object isEmpty0 extends ToIsEmptyOps0[IsEmpty]
  object isEmpty extends ToIsEmptyOps[IsEmpty]

  object plusEmpty0 extends ToPlusEmptyOps0[PlusEmpty]
  object plusEmpty extends ToPlusEmptyOps[PlusEmpty]

  object functor0 extends ToFunctorOps0[Functor]
  object functor extends ToFunctorOps[Functor]

  object invariantFunctor0 extends ToInvariantFunctorOps0[InvariantFunctor]
  object invariantFunctor extends ToInvariantFunctorOps[InvariantFunctor]

  object contravariant0 extends ToContravariantOps0[Contravariant]
  object contravariant extends ToContravariantOps[Contravariant]

  object align0 extends ToAlignOps0[Align]
  object align extends ToAlignOps[Align]

  object apply0 extends ToApplyOps0[Apply]
  object apply extends ToApplyOps[Apply]

  object applicative0 extends ToApplicativeOps0[Applicative]
  object applicative extends ToApplicativeOps[Applicative]

  object bind0 extends ToBindOps0[Bind]
  object bind extends ToBindOps[Bind]

  object monad0 extends ToMonadOps0[Monad]
  object monad extends ToMonadOps[Monad]

  object cobind0 extends ToCobindOps0[Cobind]
  object cobind extends ToCobindOps[Cobind]

  object comonad0 extends ToComonadOps0[Comonad]
  object comonad extends ToComonadOps[Comonad]

  object cozip0 extends ToCozipOps0[Cozip]
  object cozip extends ToCozipOps[Cozip]

  object plus0 extends ToPlusOps0[Plus]
  object plus extends ToPlusOps[Plus]

  object applicativePlus0 extends ToApplicativePlusOps0[ApplicativePlus]
  object applicativePlus extends ToApplicativePlusOps[ApplicativePlus]

  object alt0 extends ToAltOps0[Alt]
  object alt extends ToAltOps[Alt]

  object monadPlus0 extends ToMonadPlusOps0[MonadPlus]
  object monadPlus extends ToMonadPlusOps[MonadPlus]

  object foldable0 extends ToFoldableOps0[Foldable]
  object foldable extends ToFoldableOps[Foldable]

  object foldable10 extends ToFoldable1Ops0[Foldable1]
  object foldable1 extends ToFoldable1Ops[Foldable1]

  object traverse0 extends ToTraverseOps0[Traverse]
  object traverse extends ToTraverseOps[Traverse]

  object traverse10 extends ToTraverse1Ops0[Traverse1]
  object traverse1 extends ToTraverse1Ops[Traverse1]

  object zip0 extends ToZipOps0[Zip]
  object zip extends ToZipOps[Zip]

  object unzip0 extends ToUnzipOps0[Unzip]
  object unzip extends ToUnzipOps[Unzip]

  object optional0 extends ToOptionalOps0[Optional]
  object optional extends ToOptionalOps[Optional]

  //
  // Type classes over * * -> *
  //

  object associative0 extends ToAssociativeOps0[Associative]
  object associative extends ToAssociativeOps[Associative]

  object bifunctor0 extends ToBifunctorOps0[Bifunctor]
  object bifunctor extends ToBifunctorOps[Bifunctor]

  object bifoldable0 extends ToBifoldableOps0[Bifoldable]
  object bifoldable extends ToBifoldableOps[Bifoldable]

  object bitraverse0 extends ToBitraverseOps0[Bitraverse]
  object bitraverse extends ToBitraverseOps[Bitraverse]

  object compose0 extends ToComposeOps0[Compose]
  object compose extends ToComposeOps[Compose]

  object profunctor0 extends ToProfunctorOps0[Profunctor]
  object profunctor extends ToProfunctorOps[Profunctor]

  object strong0 extends ToStrongOps0[Strong]
  object strong extends ToStrongOps[Strong]

  object proChoice0 extends ToProChoiceOps0[ProChoice]
  object proChoice extends ToProChoiceOps[ProChoice]

  object category0 extends ToCategoryOps0[Category]
  object category extends ToCategoryOps[Category]

  object arrow0 extends ToArrowOps0[Arrow]
  object arrow extends ToArrowOps[Arrow]

  object choice0 extends ToChoiceOps0[Choice]
  object choice extends ToChoiceOps[Choice]

  object split0 extends ToSplitOps0[Split]
  object split extends ToSplitOps[Split]

  object monadTell0 extends ToMonadTellOps0[MonadTell]
  object monadTell extends ToMonadTellOps[MonadTell]

  object monadListen0 extends ToMonadListenOps0[MonadListen]
  object monadListen extends ToMonadListenOps[MonadListen]

  object monadError0 extends ToMonadErrorOps0[MonadError]
  object monadError extends ToMonadErrorOps[MonadError]

  object applicativeError0 extends ToApplicativeErrorOps0[ApplicativeError]
  object applicativeError extends ToApplicativeErrorOps[ApplicativeError]

  //
  // Type classes over (* -> *) -> * -> *
  //

  object monadTrans extends ToMonadTransOps

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

  object eithert extends ToEitherTOps

  object nel extends ToNelOps

  object these extends ToTheseOps

  object maybe extends ToMaybeOps

  object tag extends ToTagOps

  object contT extends ToContTOps

  object const extends ToConstOps

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
  with ToEitherTOps
  with ToNelOps
  with ToTheseOps
  with ToMaybeOps
  with ToContTOps
  with ToConstOps

trait ToTypeClassOps
  extends ToSemigroupOps with ToMonoidOps with ToBandOps with ToEqualOps with ToShowOps
  with ToOrderOps with ToEnumOps with ToPlusEmptyOps0[PlusEmpty]
  with ToFunctorOps0[Functor] with ToContravariantOps0[Contravariant] with ToApplyOps0[Apply]
  with ToApplicativeOps0[Applicative] with ToBindOps0[Bind] with ToMonadOps0[Monad] with ToComonadOps0[Comonad]
  with ToBifoldableOps0[Bifoldable] with ToCozipOps0[Cozip]
  with ToPlusOps0[Plus] with ToApplicativePlusOps0[ApplicativePlus] with ToAltOps0[Alt] with ToMonadPlusOps0[MonadPlus] with ToTraverseOps0[Traverse] with ToBifunctorOps0[Bifunctor] with ToAssociativeOps0[Associative]
  with ToBitraverseOps0[Bitraverse] with ToComposeOps0[Compose] with ToCategoryOps0[Category]
  with ToArrowOps0[Arrow] with ToProfunctorOps0[Profunctor] with ToStrongOps0[Strong]
  with ToFoldableOps0[Foldable] with ToChoiceOps0[Choice] with ToSplitOps0[Split] with ToZipOps0[Zip] with ToUnzipOps0[Unzip] with ToMonadTellOps0[MonadTell] with ToMonadListenOps0[MonadListen] with ToMonadErrorOps0[MonadError] with ToApplicativeErrorOps0[ApplicativeError]
  with ToFoldable1Ops0[Foldable1] with ToTraverse1Ops0[Traverse1] with ToOptionalOps0[Optional] with ToAlignOps0[Align]
  with ToMonadTransOps with ToProChoiceOps0[ProChoice]
