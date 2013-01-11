package scalaz
package syntax
package typelevel

trait AllSyntaxes extends HLists with TypeClasses

trait Syntaxes {
  object hlist extends HLists
  object typeclass extends TypeClasses

  object all extends AllSyntaxes
}
