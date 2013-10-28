package scalaz
package syntax
package typelevel

import scalaz.typelevel.{HList, HCons}

trait HLists {

  type ::[H, T <: HList] = HCons[H, T]
  val :: = HCons

}

// vim: expandtab:ts=2:sw=2
