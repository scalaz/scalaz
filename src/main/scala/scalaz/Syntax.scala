package scalaz

trait Syntaxes {
  object functor extends ToFunctorSyntax
  object pointed extends ToPointedSyntax
  object apply extends ToApplySyntax
  object applicative extends ToApplicativeSyntax
  object bind extends ToBindSyntax
  object monad extends ToMonadSyntax
  object traverse extends ToTraverseSyntax
}

trait SyntaxV[A] { def self: A }

object Syntax extends Syntaxes
