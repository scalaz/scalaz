package scalaz
package typelevel
package syntax

trait AllSyntaxes extends HLists with TypeClasses with Nats

trait Syntaxes {
  object hlist extends HLists
  object typeclass extends TypeClasses
  object nat extends Nats

  object all extends AllSyntaxes
}
