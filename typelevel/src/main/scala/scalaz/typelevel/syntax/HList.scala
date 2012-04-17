package scalaz
package typelevel
package syntax

trait HLists {

  type ::[H, T <: HList] = HCons[H, T]
  def :: = HCons

}

object HLists extends HLists

// vim: expandtab:ts=2:sw=2
