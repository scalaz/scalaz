package scalaz
package typelevel
package syntax

trait Syntaxes {
  object hlist extends HLists
  object typeclass extends TypeClasses

  object all extends HLists with TypeClasses
}
