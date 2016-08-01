package scalaz
package data

import scala.language.experimental.macros

import HList._
import Telephoto._

trait OpticFunctions {
  def telephoto[
    O0, O1,
    O
  ](
    o0: O0, o1: O1
  )(implicit OL: Focal.Aux[
    HCons[O1, HCons[O0, HNil]],
    O
  ]): O =
    macro TelephotoMacros.compose2[O0, O1, O, Focal[HCons[O1, HCons[O0, HNil]]]]

  def telephotoA[
    O0, O1, O2, O3, O4, O5, O6, O7, O8, O9,
    O
  ](
    o0: O0, o1: O1, o2: O2, o3: O3, o4: O4, o5: O5, o6: O6, o7: O7, o8: O8, o9: O9
  )(implicit OL: Focal.Aux[
    HCons[O9, HCons[O8, HCons[O7, HCons[O6, HCons[O5, HCons[O4, HCons[O3, HCons[O2, HCons[O1, HCons[O0, HNil]]]]]]]]]],
    O
  ]): O =
    macro TelephotoMacros.composeA[O0, O1, O2, O3, O4, O5, O6, O7, O8, O9, O, Focal[HCons[O9, HCons[O8, HCons[O7, HCons[O6, HCons[O5, HCons[O4, HCons[O3, HCons[O2, HCons[O1, HCons[O0, HNil]]]]]]]]]]]]

}

