package scalaz
package syntax

trait Syntaxes {
  object semigroup extends ToSemigroupSyntax
  object monoid extends ToMonoidSyntax
  object functor extends ToFunctorSyntax
  object pointed extends ToPointedSyntax
  object apply extends ToApplySyntax
  object applicative extends ToApplicativeSyntax
  object bind extends ToBindSyntax
  object monad extends ToMonadSyntax
  object traverse extends ToTraverseSyntax
  object all extends ToAllSyntax
}

trait ToAllSyntax
  extends ToSemigroupSyntax with ToMonoidSyntax with ToFunctorSyntax with ToPointedSyntax with ToApplySyntax with ToApplicativeSyntax with ToBindSyntax with ToMonadSyntax with ToTraverseSyntax

trait SyntaxV[A] { def self: A }

object Syntax extends Syntaxes
