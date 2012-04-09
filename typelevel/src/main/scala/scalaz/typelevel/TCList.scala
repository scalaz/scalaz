package scalaz
package typelevel

import syntax.HLists._

sealed trait TCList {
  type λ[α] <: HList
  type Composed[α]
}

sealed trait TCCons[M[_], T <: TCList] extends TCList {
  override type λ[α] = M[α] :: T#λ[α]
  override type Composed[α] = M[T#Composed[α]]
}

sealed trait TCNil extends TCList {
  override type λ[α] = HNil
  type Composed[α] = α
}

// vim: expandtab:ts=2:sw=2
