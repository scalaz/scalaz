package scalaz
package typelevel

// This should be `package object syntax`, but is not, because that would lead
// to an 'illegal cyclic reference' in `syntax.TypeClass`.

trait Syntax
  extends syntax.HLists
  with syntax.TypeClasses

object Syntax extends Syntax

// vim: expandtab:ts=2:sw=2
