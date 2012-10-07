package scalaz
package typelevel
package syntax

trait AllSyntaxes extends HLists with TypeClasses

trait Syntaxes {
  object hlist extends HLists
  object typeclass extends TypeClasses

  object all extends AllSyntaxes
}
