package scalaz
package typelevel
package syntax

trait HLists {

  type ::[H, T <: HList] = HCons[H, T]
  def :: = HCons

}

// vim: expandtab:ts=2:sw=2
