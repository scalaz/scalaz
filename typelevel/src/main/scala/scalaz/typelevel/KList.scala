package scalaz
package typelevel

import Typelevel._

trait KLists {

  type KList[M[_]] = GenericList[M]
  type KCons[M[_], H, +T <: KList[M]] = GenericCons[M, H, T]
  type KNil[M[_]] = GenericNil[M]

  object _KNil extends KNil[Nothing]

  // This is here to force that `KNil` is of type `KNil.type`,
  // and not of `object KNil`. Otherwise, there will be a type
  // mismatch if used in a pattern match.
  val KNil: _KNil.type = _KNil

  def :^: = GenericCons

}

// vim: expandtab:ts=2:sw=2

