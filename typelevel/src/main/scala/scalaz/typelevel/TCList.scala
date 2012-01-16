package scalaz
package typelevel

import Typelevel._

sealed trait TCList {
  type λ[α] <: HList
}

sealed trait TCCons[M[_], T <: TCList] extends TCList {
  override type λ[α] = M[α] :: T#λ[α]
}

sealed trait TCNil extends TCList {
  override type λ[α] = HNil
}

// vim: expandtab:ts=2:sw=2

