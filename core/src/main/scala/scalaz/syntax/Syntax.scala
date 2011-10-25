package scalaz
package syntax

import std.{ToStreamV, ToOptionV}

trait Syntaxes {

  object semigroup extends ToSemigroupSyntax

  object monoid extends ToMonoidSyntax

  object equal extends ToEqualSyntax

  object length extends ToLengthSyntax

  object show extends ToShowSyntax

  object order extends ToOrderSyntax

  object metricSpace extends ToMetricSpaceSyntax

  object empty extends ToEmptySyntax

  object each extends ToEachSyntax

  object index extends ToIndexSyntax

  object functor extends ToFunctorSyntax

  object pointed extends ToPointedSyntax

  object contravariant extends ToContravariantSyntax

  object copointed extends ToCopointedSyntax

  object apply extends ToApplySyntax

  object applicative extends ToApplicativeSyntax

  object bind extends ToBindSyntax

  object monad extends ToMonadSyntax

  object cojoin extends ToCojoinSyntax

  object comonad extends ToComonadSyntax

  object plus extends ToPlusSyntax

  object applicativePlus extends ToApplicativePlusSyntax

  object monadPlus extends ToMonadPlusSyntax

  object traverse extends ToTraverseSyntax

  object biFunctor extends ToBiFunctorSyntax

  object biTraverse extends ToBiTraverseSyntax

  object arrId extends ToArrIdSyntax

  object arr extends ToArrSyntax

  object compose extends ToComposeSyntax

  object category extends ToCategorySyntax

  object first extends ToFirstSyntax

  object arrow extends ToArrowSyntax

  object all extends ToAllTypeClassSyntax

}

trait ToAllTypeClassSyntax
  extends ToSemigroupSyntax with ToMonoidSyntax with ToEqualSyntax with ToLengthSyntax with ToShowSyntax
  with ToOrderSyntax with ToMetricSpaceSyntax with ToEmptySyntax with ToEachSyntax with ToIndexSyntax
  with ToFunctorSyntax with ToPointedSyntax with ToContravariantSyntax with ToCopointedSyntax with ToApplySyntax
  with ToApplicativeSyntax with ToBindSyntax with ToMonadSyntax with ToCojoinSyntax with ToComonadSyntax
  with ToPlusSyntax with ToApplicativePlusSyntax with ToMonadPlusSyntax with ToTraverseSyntax with ToBiFunctorSyntax
  with ToBiTraverseSyntax with ToArrIdSyntax with ToArrSyntax with ToComposeSyntax with ToCategorySyntax
  with ToFirstSyntax with ToArrowSyntax

trait ToAllStdV
  extends ToOptionV with ToStreamV


trait SyntaxV[A] {
  def self: A
}

/**The members of this object are also offered in the package object [[scalaz.syntax]] */
object Syntax extends Syntaxes
